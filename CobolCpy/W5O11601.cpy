000100 01  MOD-W5O11601.                                                        
000200*                                 MOD-COPYTEXT FÖR W5011600               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-BEART-SVE        PIC X(25).                                   
001200*                                 SVENSK ARTIKELBENÄMNING                 
001300     03 MOD-PRINK-AKT        PIC Z(6)9.9(2).                              
001400*                                 INKÖPSPRIS AKTUELLT ÅR                  
001500     03 MOD-PRINK-KOM        PIC Z(6)9.9(2).                              
001600*                                 INKÖPSPRIS NÄSTA ÅR                     
001700     03 MOD-REAENDR-INK      PIC Z(3)9.9-.                                
001800*                                 ÄNDRINGSPROCENT                         
001900     03 MOD-IDINK            PIC X(4).                                    
002000*                                 INKÖPARNUMMER                           
002100     03 MOD-KVDISP-SPIS      PIC -(5)9.                                   
002200*                                 DISPONIBELT LAGER FÖR SPIS              
002300     03 MOD-PRARTSTD-AKT     PIC Z(6)9.9(2).                              
002400*                                 ARTIKELSTANDARDPRIS                     
002500     03 MOD-PRARTSTD-KOM     PIC Z(6)9.9(2).                              
002600*                                 ARTIKELSTANDARDPRIS                     
002700     03 MOD-REAENDR-STD      PIC Z(3)9.9-.                                
002800*                                 ÄNDRINGSPROCENT                         
002900     03 MOD-RETULF           PIC Z(2)9.9(4).                              
003000*                                 TULLFAKTOR                              
003100     03 MOD-PRARTBES         PIC Z(6)9.9(2).                              
003200*                                 BESTÄLLNINGSPRIS I KRONOR               
003300     03 MOD-PRKURS           PIC Z(5)9.9(5).                              
003400*                                 VALUTAKURS                              
003500     03 MOD-PRARTSJK         PIC Z(6)9.9(2).                              
003600*                                 ARTIKELNS SJÄLVKOSTNAD                  
003700     03 MOD-FLIART           PIC X.                                       
003800*                                 ARTIKELN INGÅR I SATS                   
003900     03 MOD-PRDIRLON-AKT     PIC Z(3)9.9(3).                              
004000*                                 DIREKT LÖN AKTUELL                      
004100     03 MOD-PRDIRLON-KOM     PIC Z(3)9.9(3).                              
004200*                                 DIREKT LÖN AKTUELL                      
004300     03 MOD-REAENDR-DL       PIC Z(3)9.9-.                                
004400*                                 ÄNDRINGSPROCENT                         
004500     03 MOD-IDLEVNR-HUV      PIC X(5).                                    
004600*                                 LEVERANTÖRNR HUVUDLEVERANTÖR            
004700     03 MOD-PRDMTRL-AKT      PIC Z(5)9.9(3).                              
004800*                                 DIREKT MATERIAL DETTA ÅR                
004900     03 MOD-PRDMTRL-KOM      PIC Z(5)9.9(3).                              
005000*                                 DIREKT MATERIAL DETTA ÅR                
005100     03 MOD-REAENDR-MTRL     PIC Z(3)9.9-.                                
005200*                                 ÄNDRINGSPROCENT                         
005300     03 MOD-KDPRODSL         PIC Z9.                                      
005400*                                 PRODUKTSLAG                             
005500     03 MOD-PROVRPAL-AKT     PIC Z(3)9.9(3).                              
005600*                                 ÖVRIGA OMKOSTNADER PÅLÄGG,              
005700*                                 DETTA ÅR                                
005800     03 MOD-PROVRPAL-KOM     PIC Z(3)9.9(3).                              
005900*                                 ÖVRIGA OMKOSTNADER PÅLÄGG,              
006000*                                 DETTA ÅR                                
006100     03 MOD-REAENDR-OVR      PIC Z(3)9.9-.                                
006200*                                 ÄNDRINGSPROCENT                         
006300     03 MOD-TIPRLIST-UTM     PIC 9(6).                                    
006400*                                 PRISLISTEDATUM (AAMMDD)                 
006500     03 MOD-IDLEVNR-UTM      PIC X(5).                                    
006600*                                 LEVERANTÖRNUMMER                        
006700     03 MOD-PRARTBEL-PR-UTM  PIC Z(7)9.9(5).                              
006800*                                 DETTA BESTÄLLNINGSPRIS                  
006900*                                 (I LEVERANTÖRENS VALUTA)                
007000     03 MOD-KDVALISO-UTM     PIC X(3).                                    
007100*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
007200     03 MOD-KDSTASPIS        PIC X(5).                                    
007300*                                 STATUS BESTÄLLNINGSPRIS                 
007400     03 MOD-KDPRIBEH         PIC X.                                       
007500*                                 PRISBEHANDLINGSKOD                      
007600*                                  B = BORTTAGSMARKERAD. BEH EJ           
007700*                                  J = UPPDATERAS DIREKT                  
007800*                                  N = BEHANDLAS EJ. EJ KONTROLL.         
007900*                                  V = BEHANDLAS I VECKOKÖRNINGEN         
008000     03 MOD-REDIRLEV         PIC 9.9(2).                                  
008100*                                 DIREKTLEVERANSANDEL                     
008200     03 MOD-FLPRFIL          PIC X.                                       
008300*                                 PRISHÄMTNINGSFLAGGA                     
008400     03 MOD-KDPRIBEH-IN-ATTR PIC X(2).                                    
008500*                                 MFS ATTRIBUTFÄLT                        
008600     03 MOD-KDPRIBEH-IN      PIC X.                                       
008700*                                 PRISBEHANDLINGSKOD                      
008800*                                  B = BORTTAGSMARKERAD. BEH EJ           
008900*                                  J = UPPDATERAS DIREKT                  
009000*                                  N = BEHANDLAS EJ. EJ KONTROLL.         
009100*                                  V = BEHANDLAS I VECKOKÖRNINGEN         
009200     03 MOD-DAREGDAT         PIC 9(8).                                    
009300*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
009400     03 MOD-TIREGTID         PIC 9(6).                                    
009500*                                 REGISTRERINGSTID                        
009600     03 MOD-TEMFSINF         PIC X(55).                                   
009700*                                 INFORMATIONSMEDDELANDE                  
009800*** END OF VILMAII-COPY LENGTH= 383 BYTES                                 
