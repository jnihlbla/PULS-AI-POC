000100 01  W5O10401.                                                            
000200*                                 COPYTEXT FÖR MOD W5O10401               
000300     03 IDTRANS              PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 TEMFSFEL             PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 IDARTNR-IN           PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 IDARTNR-UT           PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 BINDESTRECK          PIC X.                                       
001200     03 AREA.                                                             
001300        05 REKSIFFR          PIC 9.                                       
001400*                                 KONTROLLSIFFRA                          
001500        05 PRINK             PIC Z(6)9.9(2).                              
001600*                                 INKÖPSPRIS                              
001700        05 PRARTSTD          PIC Z(6)9.9(2).                              
001800*                                 ARTIKELSTANDARDPRIS                     
001900        05 PRARTBES          PIC Z(6)9.9(2).                              
002000*                                 BESTÄLLNINGSPRIS I KRONOR               
002100        05 PRARTSJK          PIC Z(6)9.9(2).                              
002200*                                 ARTIKELNS SJÄLVKOSTNAD                  
002300        05 PRDIRLON          PIC Z(3)9.9(3).                              
002400*                                 DIREKT LÖN                              
002500        05 PRDMTRL           PIC Z(5)9.9(3).                              
002600*                                 DIREKT MATERIAL                         
002700        05 PROVRPAL          PIC Z(3)9.9(3).                              
002800*                                 ÖVRIGA OMKOSTNADER PÅLÄGG               
002900        05 KDTIPPR           PIC X.                                       
003000*                                 TIPPAT PRIS KOD                         
003100        05 IDANSK            PIC Z(2)9.                                   
003200*                                 ANSKAFFARNUMMER                         
003300        05 IDINK             PIC X(4).                                    
003400*                                 INKÖPARNUMMER                           
003500        05 PRIS-BEST         OCCURS 6 TIMES                               
003600                             INDEXED IX-PB.                               
003700           07 TIPRLIST       PIC 9(6)B(2).                                
003800*                                 PRISLISTEDATUM (AAMMDD)                 
003900           07 IDLEVNR-PR     PIC X(5).                                    
004000*                                 LEVERANTÖRNUMMER                        
004100           07 FILLER         PIC X(5).                                    
004200           07 PRARTBES-PR    PIC Z(6)9.9(2)B(2).                          
004300*                                 BESTÄLLNINGSPRIS I KRONOR               
004400           07 PRARTBEL-PR    PIC Z(7)9.9(5)B(3).                          
004500*                                 BESTPRIS LEVERANTÖRENS VALUTA           
004600           07 KDSTATUS-PR    PIC X(5).                                    
004700        05 BESTALLNING       OCCURS 4 TIMES                               
004800                             INDEXED IX-BE.                               
004900           07 IDBEST         PIC Z9(12).                                  
005000*                                 BESTÄLLNINGS-ID (PPPBBBBBBSSS)          
005100*                                 PPP   = (PREFIX) INKÖPARNR              
005200*                                 BBBBBB= BESTÄLLARNR                     
005300*                                 SSS   = (SUFFIX) GODSM/PROD.KOD         
005400           07 FILLER         PIC X(6).                                    
005500           07 IDLEVNR-B      PIC X(5).                                    
005600*                                 LEVERANTÖRNUMMER                        
005700           07 FILLERX2       PIC X(2).                                    
005800           07 TIBEST         PIC 9(6).                                    
005900*                                 BESTÄLLNINGSDATUM (ÅÅMMDD)              
006000           07 KVBEST         PIC Z(7)9.                                   
006100*                                 BESTÄLLT ANTAL                          
006200           07 KVBEST-BEKR    PIC Z(7)9B.                                  
006300*                                 BEKRÄFTAT BESTÄLLT ANTAL                
006400           07 KDJUST         PIC X(8).                                    
006500        05 TEMFSINF          PIC X(55).                                   
006600*                                 INFORMATIONSMEDDELANDE                  
006700*** END OF VILMAII-COPY LENGTH= 733 BYTES                                 
