000100 01  RESP-WL0129O2.                                                       
000200*                                 RESPONS FROM PGM WL0129                 
000300*                                 PRINTING OF DELIVERY NOTE               
000400     03 RESP-RAD1.                                                        
000500        05 RESP-REP-IDPTYP-1 PIC X(3).                                    
000600*                                 POSTTYP                                 
000700        05 RESP-REP-IDDC     PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900        05 RESP-REP-IDDISTR  PIC Z(3)9.                                   
001000*                                 DISTRIKTNUMMER                          
001100        05 RESP-REP-IDKUNDNR PIC Z(5)9.                                   
001200*                                 KUNDNUMMER                              
001300        05 RESP-REP-IDORDNR  PIC Z(4)9.                                   
001400*                                 ORDERNUMMER UTGÅR PD90                  
001500        05 RESP-REP-KDORDKL  PIC 9.                                       
001600*                                 ORDERKLASS                              
001700        05 RESP-REP-IDBORD   PIC X(3).                                    
001800*                                 PACK-BORD                               
001900        05 RESP-REP-IDUSER   PIC X(8).                                    
002000*                                 ANVÄNDARENS SÄKERHETS ID                
002100        05 RESP-REP-IDPRODNR PIC Z(6)9.                                   
002200*                                 PRODUKTIONSNUMMER                       
002300        05 RESP-REP-BEGMT-RAD1                                            
002400                             PIC X(35).                                   
002500*                                 GODSMOTTAGARNAMN RAD 1                  
002600        05 RESP-REP-BEGMT-RAD2                                            
002700                             PIC X(35).                                   
002800*                                 GODSMOTTAGARNAMN RAD 2                  
002900        05 RESP-REP-ADGMT-GATA                                            
003000                             PIC X(35).                                   
003100*                                 GODSMOTTAGARADRESS GATA                 
003200        05 RESP-REP-ADGMT-PADR                                            
003300                             PIC X(35).                                   
003400*                                 GODSMOTTAGARADRESS POSTADRESS           
003500        05 RESP-REP-ADGMT-LAND                                            
003600                             PIC X(35).                                   
003700*                                 GODSMOTTAGARADRESS LAND                 
003800        05 RESP-REP-BEKUNDRF PIC X(15).                                   
003900*                                 KUNDENS REFERENS                        
004000        05 RESP-REP-TIRFSDAT PIC 9(6).                                    
004100*                                 KLART FÖR TRANSPORT ÅÅMMDD              
004200        05 RESP-REP-TIRFSTID PIC 9(4).                                    
004300*                                 KLART FÖR TRANSPORT (TTMM)              
004400        05 RESP-REP-IDKOLLI-FOM                                           
004500                             PIC Z(4)9.                                   
004600*                                 KOLLINUMMER FRÅN OCH MED                
004700        05 RESP-REP-IDKOLLI-TOM                                           
004800                             PIC Z(4)9.                                   
004900*                                 KOLLINUMMER TILL OCH MED                
005000        05 RESP-REP-TIPACKN  PIC 9(6).                                    
005100*                                 PACKNINGSDATUM         (ÅÅMMDD)         
005200        05 RESP-REP-TIPACTID PIC 9(4).                                    
005300*                                 PACKNINGSTID  TTMMSS                    
005400     03 RESP-RAD2 REDEFINES RESP-RAD1.                                    
005500        05 RESP-REP-IDPTYP-2 PIC X(3).                                    
005600*                                 POSTTYP                                 
005700        05 RESP-REP-IDORDNR-RO                                            
005800                             PIC Z(4)9.                                   
005900*                                 ORDERNUMMER UTGÅR PD90                  
006000        05 RESP-REP-IDARTNR  PIC Z(7)9.                                   
006100*                                 ARTIKELNUMMER                           
006200        05 RESP-REP-REKSIFFR PIC 9.                                       
006300*                                 KONTROLLSIFFRA                          
006400        05 RESP-REP-BERADREF PIC X(10).                                   
006500*                                 KUNDENS RADREFERENS                     
006600        05 RESP-REP-IDARTNR-ERS                                           
006700                             PIC Z(7)9.                                   
006800*                                 ERSATT ARTIKELNUMMER                    
006900        05 RESP-REP-BEART    PIC X(25).                                   
007000*                                 ARTIKELBENÄMNING                        
007100        05 RESP-REP-KVLEVART PIC Z(6)9.                                   
007200*                                 LEVERERAT ANTAL STYCK                   
007300        05 RESP-FILLER       PIC X(192).                                  
007400     03 RESP-RAD3 REDEFINES RESP-RAD1.                                    
007500        05 RESP-REP-IDPTYP-3 PIC X(3).                                    
007600*                                 POSTTYP                                 
007700        05 RESP-REP-VLORDBTO PIC Z(3)9.9(3).                              
007800*                                 ORDERVOLYM BRUTTO (M3)                  
007900        05 RESP-REP-VKORDBTO PIC Z(5)9.9.                                 
008000*                                 ORDERVIKT BRUTTO (KG)                   
008100        05 RESP-REP-SUM-ART  PIC Z(6)9.                                   
008200*                                 ANTAL ARTNR PER BRYTBEGREPP             
008300        05 RESP-FILLER       PIC X(233).                                  
008400*** END OF VILMAII-COPY LENGTH= 259 BYTES                                 
