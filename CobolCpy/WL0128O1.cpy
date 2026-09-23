000100 01  RESP-WL0128O1.                                                       
000200*                                 RESPONS FROM PGM WL0128                 
000300*                                 PRINTING CASE LABEL                     
000400     03 RESP-RAD-NYCKLAR.                                                 
000500        05 RESP-L128-IDDC-KEY                                             
000600                             PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800        05 RESP-L128-IDDISTR-KEY                                          
000900                             PIC Z(3)9.                                   
001000*                                 DISTRIKTNUMMER                          
001100        05 RESP-L128-IDKUNDNR-KEY                                         
001200                             PIC Z(5)9.                                   
001300*                                 KUNDNUMMER                              
001400        05 RESP-L128-IDORDNR-KEY                                          
001500                             PIC Z(4)9.                                   
001600*                                 ORDERNUMMER UTGÅR PD90                  
001700        05 RESP-L128-IDKOLLI-KEY                                          
001800                             PIC Z(4)9.                                   
001900*                                 KOLLINUMMER                             
002000        05 RESP-L128-IDPRODNR-KEY                                         
002100                             PIC Z(6)9.                                   
002200*                                 PRODUKTIONSNUMMER                       
002300     03 RESP-RAD2.                                                        
002400        05 RESP-L128-ADFLGEO PIC X(3).                                    
002500*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
002600        05 RESP-L128-ADFLOMR PIC Z(2)9.                                   
002700*                                 LASTNINGSOMR/STÄLL FÄRDIGLAGER          
002800        05 RESP-L128-ADRUTNIV                                             
002900                             PIC Z(2)9.                                   
003000*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
003100        05 RESP-L128-ADVMODUL                                             
003200                             PIC Z(3).                                    
003300*                                 VÄNSTER-MODUL                           
003400        05 RESP-L128-ADHMODUL                                             
003500                             PIC Z(3).                                    
003600*                                 HÖGER-MODUL                             
003700        05 RESP-L128-KDFRAKT PIC Z9.                                      
003800*                                 FRAKTSÄTT DC TILL KUND                  
003900     03 RESP-WL0128O2.                                                    
004000*                                 RESPONS FROM PGM WL0128                 
004100*                                 PRINTING OF CASE LABEL                  
004200        05 RESP-REP-KVRADER  PIC 9(5).                                    
004300*                                 ANTAL RADER                             
004400        05 RESP-REP-RAD      OCCURS 100 TIMES.                            
004500           07 RESP-REP-IDPTYP                                             
004600                             PIC X(3).                                    
004700*                                 POSTTYP                                 
004800           07 RESP-REP-IDDC  PIC X(2).                                    
004900*                                 IDENTIFIERARE LAGER                     
005000           07 RESP-REP-IDDISTR                                            
005100                             PIC Z(3)9.                                   
005200*                                 DISTRIKTNUMMER                          
005300           07 RESP-REP-IDKUNDNR                                           
005400                             PIC Z(5)9.                                   
005500*                                 KUNDNUMMER                              
005600           07 RESP-REP-IDORDNR                                            
005700                             PIC Z(4)9.                                   
005800*                                 ORDERNUMMER UTGÅR PD90                  
005900           07 RESP-REP-IDLOPNR-ORD                                        
006000                             PIC 9(3).                                    
006100*                                 ORDERNS ORDNINGSNUMMER INOM             
006200*                                 EN PLOCKSATS                            
006300           07 RESP-REP-IDKOLLI                                            
006400                             PIC Z(4)9.                                   
006500*                                 KOLLINUMMER                             
006600           07 RESP-REP-IDPRODNR                                           
006700                             PIC Z(6)9.                                   
006800*                                 PRODUKTIONSNUMMER                       
006900           07 RESP-REP-ADFLGEO                                            
007000                             PIC X(3).                                    
007100*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
007200           07 RESP-REP-ADFLOMR                                            
007300                             PIC Z(2)9.                                   
007400*                                 LASTNINGSOMR/STÄLL FÄRDIGLAGER          
007500           07 RESP-REP-ADRUTNIV                                           
007600                             PIC Z(2)9.                                   
007700*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
007800           07 RESP-REP-BEGMT-RAD1                                         
007900                             PIC X(35).                                   
008000*                                 GODSMOTTAGARNAMN RAD 1                  
008100           07 RESP-REP-BEGMT-RAD2                                         
008200                             PIC X(35).                                   
008300*                                 GODSMOTTAGARNAMN RAD 2                  
008400           07 RESP-REP-ADGMT-GATA                                         
008500                             PIC X(35).                                   
008600*                                 GODSMOTTAGARADRESS GATA                 
008700           07 RESP-REP-ADGMT-PADR                                         
008800                             PIC X(35).                                   
008900*                                 GODSMOTTAGARADRESS POSTADRESS           
009000           07 RESP-REP-ADGMT-LAND                                         
009100                             PIC X(35).                                   
009200*                                 GODSMOTTAGARADRESS LAND                 
009300           07 RESP-REP-TIRFSDAT                                           
009400                             PIC 9(6).                                    
009500*                                 KLART FÖR TRANSPORT ÅÅMMDD              
009600           07 RESP-REP-KDFRAKT                                            
009700                             PIC Z9.                                      
009800*                                 FRAKTSÄTT DC TILL KUND                  
009900           07 RESP-REP-BERADREF                                           
010000                             PIC X(10).                                   
010100*                                 KUNDENS RADREFERENS                     
010200           07 RESP-REP-BEKUNDRF                                           
010300                             PIC X(15).                                   
010400*                                 KUNDENS REFERENS                        
010500           07 RESP-REP-IDDEPT                                             
010600                             PIC 9(2).                                    
010700*                                 AVDELNING I VERKSTAD                    
010800           07 RESP-REP-VKORDBTO                                           
010900                             PIC Z(5)9.9.                                 
011000*                                 ORDERVIKT BRUTTO (KG)                   
011100           07 RESP-REP-BARCODE                                            
011200                             PIC X(22).                                   
011300           07 RESP-REP-RESTORDER                                          
011400                             PIC X(2).                                    
011500           07 RESP-REP-KDORDKL                                            
011600                             PIC 9.                                       
011700*                                 ORDERKLASS                              
011800           07 RESP-REP-KDKOLLI                                            
011900                             PIC X(8).                                    
012000*                                 KOLLIKOD                                
012100           07 RESP-REP-IDBILREG                                           
012200                             PIC X(10).                                   
012300*                                 BILENS REGISTRERINGSNUMMER              
012400           07 RESP-REP-TIREPDAT                                           
012500                             PIC 9(6).                                    
012600*                                 REPAIR DATE                             
012700           07 RESP-REP-KUNDINFO-RAD1                                      
012800                             PIC X(35).                                   
012900*                                 GODSMOTTAGARNAMN RAD 1                  
013000           07 RESP-REP-KUNDINFO-RAD2                                      
013100                             PIC X(35).                                   
013200*                                 GODSMOTTAGARNAMN RAD 2                  
013300           07 RESP-REP-BEMEKAN                                            
013400                             PIC X(15).                                   
013500*                                 FÖRVALD MEKANIKER/VERKSTAD              
013600           07 RESP-REP-FLFPLOCK                                           
013700                             PIC X.                                       
013800*                                 FÖRLEVERANSINDIKATOR                    
013900           07 RESP-REP-TETACDBO                                           
014000                             PIC X(35).                                   
014100*                                 REFERENS BUTIK ORDER TACDIS             
014200           07 RESP-REP-BETELNR-TACD                                       
014300                             PIC X(25).                                   
014400*                                 TELEFONNUMMER SMS BUTIKSORDER           
014500           07 RESP-REP-FLTACDISKND                                        
014600                             PIC X.                                       
014700           07 RESP-REP-HOUR-TACDIS                                        
014800                             PIC X(2).                                    
014900           07 RESP-REP-MINUTE-TACDIS                                      
015000                             PIC X(2).                                    
015100           07 RESP-REP-IDARTNR                                            
015200                             PIC Z(7)9.                                   
015300*                                 ARTIKELNUMMER                           
015400           07 RESP-REP-IDTRPTNR                                           
015500                             PIC Z(2)9.                                   
015600*                                 TRANSPORTIDENTITET                      
015700*** END OF VILMAII-COPY LENGTH= 47351 BYTES                               
