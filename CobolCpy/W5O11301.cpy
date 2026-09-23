000100 01  MOD-W5O11301.                                                        
000200*                                 MOD-COPYTEXT FÖR W5011300               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(2).                                    
000800*                                 MFS BEHANDLING AV INPUTFÄLT             
000900     03 MOD-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-REKSIFFR         PIC 9.                                       
001200*                                 KONTROLLSIFFRA                          
001300     03 MOD-IDANSK           PIC Z(2)9.                                   
001400*                                 ANSKAFFARNUMMER                         
001500     03 MOD-IDINK            PIC X(4).                                    
001600*                                 INKÖPARNUMMER                           
001700     03 MOD-PRHEMTAG         PIC Z(6)9.9(2).                              
001800*                                 HEMTAGNINGSKOSTNAD                      
001900     03 MOD-KDPRODSL         PIC 9(2).                                    
002000*                                 PRODUKTSLAG                             
002100     03 MOD-KDTIPPR          PIC 9.                                       
002200*                                 TIPPAT PRIS KOD                         
002300     03 MOD-PRARTBES         PIC Z(6)9.9(2).                              
002400*                                 BESTÄLLNINGSPRIS I KRONOR               
002500     03 MOD-PRARTSJK         PIC Z(6)9.9(2).                              
002600*                                 ARTIKELNS SJÄLVKOSTNAD                  
002700     03 MOD-PRARTSTD         PIC Z(6)9.9(2).                              
002800*                                 ARTIKELSTANDARDPRIS                     
002900     03 MOD-PRINK            PIC Z(6)9.9(2).                              
003000*                                 INKÖPSPRIS                              
003100     03 MOD-PRDIRLON         PIC Z(3)9.9(3).                              
003200*                                 DIREKT LÖN                              
003300     03 MOD-PRDMTRL          PIC Z(5)9.9(3).                              
003400*                                 DIREKT MATERIAL                         
003500     03 MOD-PROVRPAL         PIC Z(3)9.9(3).                              
003600*                                 ÖVRIGA OMKOSTNADER PÅLÄGG               
003700     03 MOD-PRIS-BEST        OCCURS 6 TIMES.                              
003800        05 MOD-TIPRLIST      PIC 9(6)B(2).                                
003900*                                 PRISLISTEDATUM (AAMMDD)                 
004000        05 MOD-IDLEVNR-PR    PIC X(5).                                    
004100*                                 LEVERANTÖRNR FÖR DETTA PRIS             
004200        05 MOD-FILLER        PIC X(3).                                    
004300        05 MOD-PRARTBES-PR   PIC Z(6)9.9(2)B(2).                          
004400*                                 DETTA BESTÄLLNINGSPRIS (KR)             
004500        05 MOD-PRARTBEL-PR   PIC Z(7)9.9(5).                              
004600*                                 DETTA BESTÄLLNINGSPRIS                  
004700*                                 (I LEVERANTÖRENS VALUTA)                
004800        05 MOD-FILLER        PIC X(3).                                    
004900        05 MOD-STATUS-FAELT  PIC X(5).                                    
005000     03 MOD-BESTALLNING      OCCURS 4 TIMES.                              
005100        05 MOD-IDBEST        PIC Z9(12).                                  
005200*                                 BESTÄLLNINGS-ID (PPPBBBBBBSSS)          
005300*                                 PPP   = (PREFIX) INKÖPARNR              
005400*                                 BBBBBB= BESTÄLLARNR                     
005500*                                 SSS   = (SUFFIX) GODSM/PROD.KOD         
005600        05 MOD-FILLER        PIC X(6).                                    
005700        05 MOD-IDLEVNR-BEST  PIC X(5).                                    
005800*                                 LEVERANTÖR ENL. BESTÄLLNING             
005900        05 MOD-FILLER        PIC X(2).                                    
006000        05 MOD-TIBEST        PIC 9(6).                                    
006100*                                 BESTÄLLNINGSDATUM (ÅÅMMDD)              
006200        05 MOD-KVBEST        PIC Z(7)9.                                   
006300*                                 BESTÄLLT ANTAL                          
006400        05 MOD-KVBEST-BEKR   PIC Z(7)9B.                                  
006500*                                 BEKRÄFTAT BESTÄLLT ANTAL                
006600        05 MOD-JUST-FAELT    PIC X(8).                                    
006700     03 MOD-TEMFSINF         PIC X(55).                                   
006800*                                 INFORMATIONSMEDDELANDE                  
006900*** END OF VILMAII-COPY LENGTH= 725 BYTES                                 
