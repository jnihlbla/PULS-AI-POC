000100 01  MOD-W5O11401.                                                        
000200*                                 MOD-COPYTEXT FÖR W5011400               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-IDLEVNR-IN       PIC X(5).                                    
001200*                                 LEVERANTÖRNUMMER                        
001300     03 MOD-IDLEVNR-UT       PIC X(5).                                    
001400*                                 LEVERANTÖRNUMMER                        
001500     03 MOD-KDPRBEH-IN       PIC X.                                       
001600*                                 PRIS BEHANDLAD ARTIKEL                  
001700     03 MOD-KDPRBEH-UT       PIC X.                                       
001800*                                 PRIS BEHANDLAD ARTIKEL                  
001900     03 MOD-REAENDR-IN       PIC Z(3)9.9.                                 
002000*                                 ÄNDRINGSPROCENT                         
002100     03 MOD-REAENDR-UT       PIC Z(3)9.9.                                 
002200*                                 ÄNDRINGSPROCENT                         
002300     03 MOD-IDARTNR-ENTER    PIC X(9).                                    
002400*                                 ARTIKELNUMMER                           
002500     03 MOD-IDLEVNR-ENTER    PIC X(5).                                    
002600*                                 LEVERANTÖRNUMMER                        
002700     03 MOD-FLAGGA           PIC X.                                       
002800*                                 ALLMÄN FLAGGA                           
002900     03 MOD-IDPRANSV         PIC X(4).                                    
003000*                                 PRISANSVAR FÖR ARTIKELN                 
003100     03 MOD-MARKNING         PIC X(6).                                    
003200     03 MOD-BEART-SVE        PIC X(25).                                   
003300*                                 SVENSK ARTIKELBENÄMNING                 
003400     03 MOD-KVBEHOVAR        PIC Z(6)9.                                   
003500*                                 BERÄKNAT ÅRSBEHOV AV EN ARTIKEL         
003600     03 MOD-PRINK-AKT-INK    PIC Z(6)9.9(2).                              
003700*                                 INKÖPSPRIS AKTUELLT ÅR                  
003800     03 MOD-PRINK-KOM-ATTR   PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000     03 MOD-PRINK-KOM-INK    PIC Z(6)9.9(2).                              
004100*                                 INKÖPSPRIS NÄSTA ÅR                     
004200     03 MOD-REAENDR-INK-ATTR PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400     03 MOD-REAENDR-INK      PIC Z(3)9.9-.                                
004500*                                 ÄNDRINGSPROCENT                         
004600     03 MOD-KVDISP-SPIS      PIC Z(5)9.                                   
004700*                                 DISPONIBELT LAGER FÖR SPIS              
004800     03 MOD-PRINK-AKT-TOT    PIC Z(6)9.9(2).                              
004900*                                 INKÖPSPRIS AKTUELLT ÅR                  
005000     03 MOD-PRINK-KOM-TOT    PIC Z(6)9.9(2).                              
005100*                                 INKÖPSPRIS NÄSTA ÅR                     
005200     03 MOD-REAENDR-TOT      PIC Z(3)9.9-.                                
005300*                                 ÄNDRINGSPROCENT                         
005400     03 MOD-RETULF-UT        PIC Z(2)9.9(4).                              
005500*                                 TULLFAKTOR                              
005600     03 MOD-RETULF-IN-ATTR   PIC X(2).                                    
005700*                                 MFS ATTRIBUTFÄLT                        
005800     03 MOD-RETULF-IN        PIC Z(2)9.9(4).                              
005900*                                 TULLFAKTOR                              
006000     03 MOD-PRARTBES         PIC Z(6)9.9(2).                              
006100*                                 BESTÄLLNINGSPRIS I KRONOR               
006200     03 MOD-PRINK-KOM-BES    PIC Z(6)9.9(2).                              
006300*                                 INKÖPSPRIS NÄSTA ÅR                     
006400     03 MOD-PRKURS-UT        PIC Z(5)9.9(5).                              
006500*                                 VALUTAKURS                              
006600     03 MOD-PRKURS-IN-ATTR   PIC X(2).                                    
006700*                                 MFS ATTRIBUTFÄLT                        
006800     03 MOD-PRKURS-IN        PIC Z(5)9.9(5).                              
006900*                                 VALUTAKURS                              
007000     03 MOD-PRARTSJK         PIC Z(6)9.9(2).                              
007100*                                 ARTIKELNS SJÄLVKOSTNAD                  
007200     03 MOD-PRINK-KOM-SJK    PIC Z(6)9.9(2).                              
007300*                                 INKÖPSPRIS NÄSTA ÅR                     
007400     03 MOD-FLIART           PIC X.                                       
007500*                                 ARTIKELN INGÅR I SATS                   
007600     03 MOD-PRDIRLON-AKT     PIC Z(3)9.9(3).                              
007700*                                 DIREKT LÖN AKTUELL                      
007800     03 MOD-PRDIRLON-KOM     PIC Z(3)9.9(3).                              
007900*                                 DIREKT LÖN NÄSTA ÅR                     
008000     03 MOD-IDLEVNR-HUV      PIC X(5).                                    
008100*                                 LEVERANTÖRNR HUVUDLEVERANTÖR            
008200     03 MOD-PRDMTRL-AKT      PIC Z(5)9.9(3).                              
008300*                                 DIREKT MATERIAL DETTA ÅR                
008400     03 MOD-PRDMTRL-KOM      PIC Z(5)9.9(3).                              
008500*                                 DIREKT MATERIAL NÄSTA ÅR                
008600     03 MOD-KDPRODSL         PIC Z9.                                      
008700*                                 PRODUKTSLAG                             
008800     03 MOD-PROVRPAL-AKT     PIC Z(3)9.9(3).                              
008900*                                 ÖVRIGA OMKOSTNADER PÅLÄGG,              
009000*                                 DETTA ÅR                                
009100     03 MOD-PROVRPAL-KOM     PIC Z(3)9.9(3).                              
009200*                                 ÖVRIGA OMKOSTNADER PÅLÄGG               
009300*                                 NÄSTA ÅR                                
009400     03 MOD-TIPRLIST-UTM     PIC 9(6).                                    
009500*                                 PRISLISTEDATUM (AAMMDD)                 
009600     03 MOD-IDLEVNR-UTM      PIC X(5).                                    
009700*                                 LEVERANTÖRNUMMER                        
009800     03 MOD-PRARTBEL-PR-UTM  PIC Z(7)9.9(5).                              
009900*                                 DETTA BESTÄLLNINGSPRIS                  
010000*                                 (I LEVERANTÖRENS VALUTA)                
010100     03 MOD-KDVALISO-UTM     PIC X(3).                                    
010200*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
010300     03 MOD-KDSTASPIS        PIC X(5).                                    
010400*                                 STATUS BESTÄLLNINGSPRIS                 
010500     03 MOD-KDPRBEH-UTM      PIC X.                                       
010600*                                 PRIS BEHANDLAD ARTIKEL                  
010700     03 MOD-REDIRLEV         PIC 9.9(2).                                  
010800*                                 DIREKTLEVERANSANDEL                     
010900     03 MOD-FLAPC            PIC X.                                       
011000*                                 APC FLAGGA                              
011100     03 MOD-FLPRFIL          PIC X.                                       
011200*                                 PRISHÄMTNINGSFLAGGA                     
011300     03 MOD-TIPRLIST-INM-ATTR                                             
011400                             PIC X(2).                                    
011500*                                 MFS ATTRIBUTFÄLT                        
011600     03 MOD-TIPRLIST-INM     PIC 9(6).                                    
011700*                                 PRISLISTEDATUM (AAMMDD)                 
011800     03 MOD-IDLEVNR-INM-ATTR PIC X(2).                                    
011900*                                 MFS ATTRIBUTFÄLT                        
012000     03 MOD-IDLEVNR-INM      PIC X(5).                                    
012100*                                 LEVERANTÖRNUMMER                        
012200     03 MOD-PRARTBEL-PR-ATTR PIC X(2).                                    
012300*                                 MFS ATTRIBUTFÄLT                        
012400     03 MOD-PRARTBEL-PR-INM  PIC Z(7)9.9(5).                              
012500*                                 DETTA BESTÄLLNINGSPRIS                  
012600*                                 (I LEVERANTÖRENS VALUTA)                
012700     03 MOD-KDVALISO-INM-ATTR                                             
012800                             PIC X(2).                                    
012900*                                 MFS ATTRIBUTFÄLT                        
013000     03 MOD-KDVALISO-INM     PIC X(3).                                    
013100*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
013200     03 MOD-KDPRBEH-INM-ATTR PIC X(2).                                    
013300*                                 MFS ATTRIBUTFÄLT                        
013400     03 MOD-KDPRBEH-INM      PIC X.                                       
013500*                                 PRIS BEHANDLAD ARTIKEL                  
013600     03 MOD-TEARTNOT-IN-UT-ATTR                                           
013700                             PIC X(2).                                    
013800*                                 MFS ATTRIBUTFÄLT                        
013900     03 MOD-TEARTNOT-IN-UT   PIC X(40).                                   
014000*                                 ARTIKEL NOTERING                        
014100     03 MOD-SVAR-VISNING     PIC X(4).                                    
014200     03 MOD-FLSVAR-ATTR      PIC X(2).                                    
014300*                                 MFS ATTRIBUTFÄLT                        
014400     03 MOD-FLSVAR-UPPDAT    PIC X.                                       
014500*                                 ALLMÄN FLAGGA                           
014600     03 MOD-TEMFSINF         PIC X(55).                                   
014700*                                 INFORMATIONSMEDDELANDE                  
014800*** END OF VILMAII-COPY LENGTH= 534 BYTES                                 
