000100 01  W5O20601.                                                            
000200*                                 COPYTEXT FÖR MOD W5O20601               
000300     03 IDTRANS              PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 TEMFSFEL             PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 IDARTNR-IN           PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 IDARTNR-UT           PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 IDLEVNR-IN           PIC X(5).                                    
001200*                                 LEVERANTÖRNUMMER                        
001300     03 IDLEVNR-UT           PIC X(5).                                    
001400*                                 LEVERANTÖRNUMMER                        
001500     03 KDPRBEH-IN           PIC X.                                       
001600*                                 PRIS BEHANDLAD ARTIKEL                  
001700     03 KDPRBEH-UT           PIC X.                                       
001800*                                 PRIS BEHANDLAD ARTIKEL                  
001900     03 REAENDR-IN           PIC Z(3)9.9.                                 
002000*                                 ÄNDRINGSPROCENT                         
002100     03 REAENDR-UT           PIC Z(3)9.9.                                 
002200*                                 ÄNDRINGSPROCENT                         
002300     03 IDDC-IN              PIC X(2).                                    
002400*                                 IDENTIFIERARE LAGER                     
002500     03 IDDC-UT              PIC X(2).                                    
002600*                                 IDENTIFIERARE LAGER                     
002700     03 IDFTG-UT             PIC 9(2).                                    
002800*                                 FÖRETAGSID EKONOM REDOVISNING           
002900     03 AREA.                                                             
003000        05 BEART             PIC X(25).                                   
003100*                                 ARTIKELBENÄMNING                        
003200        05 PRAVCOST          PIC Z(6)9.9(2).                              
003300*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
003400        05 PRMATRL           PIC Z(6)9.9(2).                              
003500*                                 FAST PRIS UNDER LÖPANDE ÅR              
003600        05 PRARTBES          PIC Z(6)9.9(2).                              
003700*                                 BESTÄLLNINGSPRIS I KRONOR               
003800        05 PRARTSJK          PIC Z(6)9.9(2).                              
003900*                                 ARTIKELNS SJÄLVKOSTNAD                  
004000        05 PRDIRLON          PIC Z(3)9.9(3).                              
004100*                                 DIREKT LÖN                              
004200        05 PRDMTRL           PIC Z(5)9.9(3).                              
004300*                                 DIREKT MATERIAL                         
004400        05 KVLS-TOT          PIC -(7)9.                                   
004500*                                 LAGERSALDO                              
004600        05 KVPB-TOT          PIC Z(6)9.9.                                 
004700*                                 PERIODBEHOV (PROGNOS)                   
004800        05 PRIS-BEST         OCCURS 5 TIMES.                              
004900           07 BEST-PRIS-ATTR PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100           07 BEST-PRIS.                                                  
005200              09 KDPRURSP-PR PIC X.                                       
005300*                                 PRISHÄRSTAMNING BESTÄLLNING             
005400              09 FILLER      PIC X.                                       
005500              09 TIPRLIST-PR PIC 9(6).                                    
005600*                                 PRISLISTEDATUM (AAMMDD)                 
005700              09 FILLER      PIC X.                                       
005800              09 IDLEVNR-PR  PIC X(5).                                    
005900*                                 LEVERANTÖRNUMMER                        
006000              09 FILLER      PIC X.                                       
006100              09 PRARTBES-PR PIC Z(6)9.9(2).                              
006200*                                 BESTÄLLNINGSPRIS I KRONOR               
006300              09 FILLER      PIC X.                                       
006400              09 PRARTBEL-PR PIC Z(7)9.9(5).                              
006500*                                 BESTPRIS LEVERANTÖRENS VALUTA           
006600              09 FILLER      PIC X(2).                                    
006700              09 KDSTATUS-PR PIC X(5).                                    
006800              09 FILLER      PIC X.                                       
006900              09 KDVALISO-PR PIC X(3).                                    
007000*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
007100              09 FILLER      PIC X.                                       
007200              09 KDFPKPRI-PR PIC X.                                       
007300*                                 OM FÖRPACKNING INGÅR I ARTPRIS          
007400        05 RETULF            PIC Z(2)9.9(4).                              
007500*                                 TULLFAKTOR                              
007600        05 UPPDAT-RAD.                                                    
007700           07 KDPRURSP-U-ATTR                                             
007800                             PIC X(2).                                    
007900*                                 MFS ATTRIBUTFÄLT                        
008000           07 KDPRURSP-U     PIC X.                                       
008100*                                 PRISHÄRSTAMNING BESTÄLLNING             
008200           07 TIPRLIST-U-ATTR                                             
008300                             PIC X(2).                                    
008400*                                 MFS ATTRIBUTFÄLT                        
008500           07 TIPRLIST-U     PIC X(6).                                    
008600*                                 PRISLISTEDATUM (AAMMDD)                 
008700           07 PRARTBEL-U-ATTR                                             
008800                             PIC X(2).                                    
008900*                                 MFS ATTRIBUTFÄLT                        
009000           07 PRARTBEL-U     PIC X(14).                                   
009100*                                 BESTPRIS LEVERANTÖRENS VALUTA           
009200           07 KDVALISO-U-ATTR                                             
009300                             PIC X(2).                                    
009400*                                 MFS ATTRIBUTFÄLT                        
009500           07 KDVALISO-U     PIC X(3).                                    
009600*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
009700           07 FLPRIBES-U-ATTR                                             
009800                             PIC X(2).                                    
009900*                                 MFS ATTRIBUTFÄLT                        
010000           07 FLPRIBES-U     PIC X.                                       
010100*                                 SKAPA EJ BESTÄLLNINGSPRIS-INFO          
010200           07 IDLEVNR-U-ATTR PIC X(2).                                    
010300*                                 MFS ATTRIBUTFÄLT                        
010400           07 IDLEVNR-U      PIC X(5).                                    
010500*                                 LEVERANTÖRNUMMER                        
010600           07 KDFPKPRI-U-ATTR                                             
010700                             PIC X(2).                                    
010800*                                 MFS ATTRIBUTFÄLT                        
010900           07 KDFPKPRI-U     PIC X.                                       
011000*                                 OM FÖRPACKNING INGÅR I ARTPRIS          
011100     03 TEMFSINF             PIC X(55).                                   
011200*                                 INFORMATIONSMEDDELANDE                  
011300*** END OF VILMAII-COPY LENGTH= 575 BYTES                                 
