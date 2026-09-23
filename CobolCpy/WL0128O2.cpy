000100 01  RESP-WL0128O2.                                                       
000200*                                 RESPONS FROM PGM WL0128                 
000300*                                 PRINTING OF CASE LABEL                  
000400     03 RESP-REP-KVRADER     PIC 9(5).                                    
000500*                                 ANTAL RADER                             
000600     03 RESP-REP-RAD         OCCURS 100 TIMES.                            
000700        05 RESP-REP-IDPTYP   PIC X(3).                                    
000800*                                 POSTTYP                                 
000900        05 RESP-REP-IDDC     PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100        05 RESP-REP-IDDISTR  PIC Z(3)9.                                   
001200*                                 DISTRIKTNUMMER                          
001300        05 RESP-REP-IDKUNDNR PIC Z(5)9.                                   
001400*                                 KUNDNUMMER                              
001500        05 RESP-REP-IDORDNR  PIC Z(4)9.                                   
001600*                                 ORDERNUMMER UTGÅR PD90                  
001700        05 RESP-REP-IDKOLLI  PIC Z(4)9.                                   
001800*                                 KOLLINUMMER                             
001900        05 RESP-REP-IDPRODNR PIC Z(6)9.                                   
002000*                                 PRODUKTIONSNUMMER                       
002100        05 RESP-REP-ADFLGEO  PIC X(3).                                    
002200*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
002300        05 RESP-REP-ADFLOMR  PIC Z(2)9.                                   
002400*                                 LASTNINGSOMR/STÄLL FÄRDIGLAGER          
002500        05 RESP-REP-ADRUTNIV PIC Z(2)9.                                   
002600*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
002700        05 RESP-REP-BEGMT-RAD1                                            
002800                             PIC X(35).                                   
002900*                                 GODSMOTTAGARNAMN RAD 1                  
003000        05 RESP-REP-BEGMT-RAD2                                            
003100                             PIC X(35).                                   
003200*                                 GODSMOTTAGARNAMN RAD 2                  
003300        05 RESP-REP-ADGMT-GATA                                            
003400                             PIC X(35).                                   
003500*                                 GODSMOTTAGARADRESS GATA                 
003600        05 RESP-REP-ADGMT-PADR                                            
003700                             PIC X(35).                                   
003800*                                 GODSMOTTAGARADRESS POSTADRESS           
003900        05 RESP-REP-ADGMT-LAND                                            
004000                             PIC X(35).                                   
004100*                                 GODSMOTTAGARADRESS LAND                 
004200        05 RESP-REP-TIRFSDAT PIC 9(6).                                    
004300*                                 KLART FÖR TRANSPORT ÅÅMMDD              
004400        05 RESP-REP-KDFRAKT  PIC Z9.                                      
004500*                                 FRAKTSÄTT DC TILL KUND                  
004600        05 RESP-REP-BERADREF PIC X(10).                                   
004700*                                 KUNDENS RADREFERENS                     
004800        05 RESP-REP-BEKUNDRF PIC X(15).                                   
004900*                                 KUNDENS REFERENS                        
005000        05 RESP-REP-IDDEPT   PIC 9(2).                                    
005100*                                 AVDELNING I VERKSTAD                    
005200        05 RESP-REP-VKORDBTO PIC Z(5)9.9.                                 
005300*                                 ORDERVIKT BRUTTO (KG)                   
005400        05 RESP-REP-BARCODE  PIC X(22).                                   
005500        05 RESP-REP-RESTORDER                                             
005600                             PIC X(2).                                    
005700        05 RESP-REP-KDORDKL  PIC 9.                                       
005800*                                 ORDERKLASS                              
005900        05 RESP-REP-KDKOLLI  PIC X(8).                                    
006000*                                 KOLLIKOD                                
006100        05 RESP-REP-IDBILREG PIC X(10).                                   
006200*                                 BILENS REGISTRERINGSNUMMER              
006300        05 RESP-REP-TIREPDAT PIC 9(6).                                    
006400*                                 REPAIR DATE                             
006500        05 RESP-REP-KUNDINFO-RAD1                                         
006600                             PIC X(35).                                   
006700*                                 GODSMOTTAGARNAMN RAD 1                  
006800        05 RESP-REP-KUNDINFO-RAD2                                         
006900                             PIC X(35).                                   
007000*                                 GODSMOTTAGARNAMN RAD 2                  
007100        05 RESP-REP-BEMEKAN  PIC X(15).                                   
007200*                                 FÖRVALD MEKANIKER/VERKSTAD              
007300        05 RESP-REP-FLFPLOCK PIC X.                                       
007400*                                 FÖRLEVERANSINDIKATOR                    
007500        05 RESP-REP-TETACDBO PIC X(35).                                   
007600*                                 REFERENS BUTIK ORDER TACDIS             
007700        05 RESP-REP-BETELNR-TACD                                          
007800                             PIC X(25).                                   
007900*                                 TELEFONNUMMER SMS BUTIKSORDER           
008000        05 RESP-REP-FLTACDISKND                                           
008100                             PIC X.                                       
008200        05 RESP-REP-HOUR-TACDIS                                           
008300                             PIC X(2).                                    
008400        05 RESP-REP-MINUTE-TACDIS                                         
008500                             PIC X(2).                                    
008600        05 RESP-REP-IDARTNR  PIC Z(7)9.                                   
008700*                                 ARTIKELNUMMER                           
008800*** END OF VILMAII-COPY LENGTH= 46705 BYTES                               
