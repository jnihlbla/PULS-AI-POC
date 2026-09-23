000100 01  RESP-WL0143O1.                                                       
000200*                                 RESPONS FROM PGM WL0143                 
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 RESP-IDDISTR-KEY     PIC Z(3)9.                                   
000600*                                 DISTRIKTNUMMER                          
000700     03 RESP-IDKUNDNR-KEY    PIC Z(5)9.                                   
000800*                                 KUNDNUMMER                              
000900     03 RESP-IDORDNR7-KEY    PIC Z(6)9.                                   
001000*                                 ORDERNUMMER                             
001100     03 RESP-IDPRODNR-KEY    PIC Z(6)9.                                   
001200*                                 PRODUKTIONSNUMMER                       
001300     03 RESP-IDKOLLI-KEY     PIC Z(5).                                    
001400*                                 KOLLINUMMER                             
001500     03 RESP-IDARTNR-KEY     PIC Z(7)9.                                   
001600*                                 ARTIKELNUMMER                           
001700     03 RESP-TEDDI           PIC X(11).                                   
001800*                                 TEXTFÄLT DDI                            
001900     03 RESP-IDSHIPM         PIC Z(6)9.                                   
002000*                                 SKEPPNINGSNUMMER                        
002100     03 RESP-KVRADER         PIC Z(4)9.                                   
002200*                                 ANTAL RADER                             
002300     03 RESP-HUVUD.                                                       
002400*                                 RAD INNEHÅLLANDE ORDERHUVUDINFO         
002500        05 RESP-KVKOLLI      PIC Z(3)9.                                   
002600*                                 ANTAL KOLLI                             
002700        05 RESP-KVKOLLI-FAKT PIC Z(3)9.                                   
002800*                                 ANTAL FAKTURERADE KOLLIN                
002900        05 RESP-KVKOLLI-LAST PIC Z(3)9.                                   
003000*                                 ANTAL LASTNINGSRAPPORTERADE             
003100*                                 KOLLIN                                  
003200        05 RESP-KVORDRAD-TOT PIC Z(4)9.                                   
003300*                                 ANTAL ORDERRADER                        
003400        05 RESP-VKORDBTO     PIC Z(5)9.9.                                 
003500*                                 ORDERVIKT BRUTTO (KG)                   
003600        05 RESP-VLORDBTO     PIC Z(3)9.9(3).                              
003700*                                 ORDERVOLYM BRUTTO (M3)                  
003800        05 RESP-SUORDV       PIC Z(8)9.9(2).                              
003900*                                 SUMMA ORDERVÄRDE                        
004000        05 RESP-TEASTRIX     PIC X.                                       
004100*                                 ASTERISK                                
004200     03 RESP-RAD             OCCURS 500 TIMES.                            
004300*                                 TABELL INNEHÅLLANDE RADER.              
004400        05 RESP-IDKOLLI      PIC Z(4)9.                                   
004500*                                 KOLLINUMMER                             
004600        05 RESP-ADKOLLI-GRP.                                              
004700*                                 ADKOLLI-GRP.                            
004800           07 RESP-ADFLGEO   PIC X(3).                                    
004900*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
005000           07 RESP-ADFLOMR   PIC X(5).                                    
005100           07 RESP-ADRUTNIV  PIC Z(2)9.                                   
005200*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
005300           07 RESP-ADVMODUL  PIC Z(3).                                    
005400*                                 VÄNSTER-MODUL                           
005500        05 RESP-TIPACKN      PIC 9(6).                                    
005600*                                 PACKNINGSDATUM         (ÅÅMMDD)         
005700        05 RESP-TIFAKT       PIC 9(6).                                    
005800*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
005900        05 RESP-TILASTN      PIC 9(6).                                    
006000*                                 LASTNINGSDATUM         (ÅÅMMDD)         
006100        05 RESP-KVORDRAD     PIC Z(4)9.                                   
006200*                                 ANTAL ORDERRADER                        
006300        05 RESP-VKORDBTO-KOLLI                                            
006400                             PIC Z(5)9.9.                                 
006500*                                 ORDERVIKT BRUTTO PER KOLLI              
006600        05 RESP-VLORDBTO-KOLLI                                            
006700                             PIC Z(3)9.9(3).                              
006800*                                 ORDERVOLYM BRUTTO KOLLI                 
006900        05 RESP-TETEXTX2     PIC X(2).                                    
007000        05 RESP-KDKOLLI      PIC X(8).                                    
007100*                                 KOLLIKOD                                
007200        05 RESP-IDPLOCK      PIC Z(5)9.                                   
007300*                                 PLOCKARE                                
007400*** END OF VILMAII-COPY LENGTH= 37108 BYTES                               
