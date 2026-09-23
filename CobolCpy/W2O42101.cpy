000100 01  MOD-W2O42101.                                                        
000200*                                 MOD COPYTEXT FÖR W2042100               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDARTNR.                                                      
001000        05 MOD-IDARTNR-UT    PIC X(9).                                    
001100*                                 ARTIKELNUMMER                           
001200        05 MOD-STRECK-1      PIC X.                                       
001300        05 MOD-REKSIFFR      PIC X.                                       
001400*                                 KONTROLLSIFFRA                          
001500     03 MOD-IDDC-IN          PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700     03 MOD-IDDC-UT          PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900     03 MOD-IDLEVNR-DC       PIC X(5).                                    
002000*                                 LEVERANTÖRNUMMER                        
002100     03 MOD-GRP-1            OCCURS 2 TIMES.                              
002200*                                 AGREEMENT                               
002300        05 MOD-IDBEST        PIC 9(12).                                   
002400*                                 BESTÄLLNINGS-ID (PPPBBBBBBSSS)          
002500*                                 PPP   = (PREFIX) INKÖPARNR              
002600*                                 BBBBBB= BESTÄLLARNR                     
002700*                                 SSS   = (SUFFIX) GODSM/PROD.KOD         
002800        05 MOD-IDLEVNR-BEST  PIC X(5).                                    
002900*                                 LEVERANTÖR ENL. BESTÄLLNING             
003000        05 MOD-TIBEST        PIC 9(6).                                    
003100*                                 BESTÄLLNINGSDATUM (ÅÅMMDD)              
003200        05 MOD-KDBEH         PIC X(15).                                   
003300     03 MOD-GRP-2            OCCURS 2 TIMES.                              
003400*                                 VALID AGREEMENTS                        
003500        05 MOD-IDAVTAL       PIC Z(11)9.                                  
003600*                                 AVTALSIDENTITET  (PPPBBBBBSSS)          
003700*                                 PPP   = INKÖPARNR (PREFIX)              
003800*                                 BBBBB = BESTÄLLARNR                     
003900*                                 SSS   = SUFFIX                          
004000        05 MOD-IDLEVNR-AVT   PIC X(5).                                    
004100*                                 LEVERANTÖR ENLIGT AVTAL                 
004200        05 MOD-IDLEVNR-SHIP  PIC X(5).                                    
004300*                                 SKEPPANDE LEVERANTÖR                    
004400        05 MOD-TIAVTAL       PIC 9(6).                                    
004500*                                 AVTALSDATUM  (ÅÅMMDD)                   
004600        05 MOD-KDFPKPRI      PIC X.                                       
004700*                                 OM FÖRPACKNING INGÅR I ARTPRIS          
004800     03 MOD-GRP-3            OCCURS 2 TIMES.                              
004900*                                 PRICE INFO                              
005000        05 MOD-IDLEVNR-PR    PIC X(5).                                    
005100*                                 LEVERANTÖRNR FÖR DETTA PRIS             
005200        05 MOD-DAPRLIST      PIC 9(6).                                    
005300        05 MOD-PRARTBES      PIC Z(6)9.9(2).                              
005400*                                 BESTÄLLNINGSPRIS I KRONOR               
005500        05 MOD-KDVALISO-PRARTBES                                          
005600                             PIC X(3).                                    
005700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
005800        05 MOD-PRARTBEL      PIC Z(6)9.9(2).                              
005900*                                 BESTÄLLNINGSPRIS   PRARTBEL-002         
006000*                                 I LEVERANTÖRS VALUTA                    
006100        05 MOD-KDVALISO-PRARTBEL                                          
006200                             PIC X(3).                                    
006300*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
006400        05 MOD-STATUS        PIC X(4).                                    
006500        05 MOD-PRMATRL       PIC Z(6)9.9(2).                              
006600*                                 FAST PRIS UNDER LÖPANDE ÅR              
006700        05 MOD-KDMATRPR      PIC X.                                       
006800     03 MOD-IDAVTAL-IN-ATTR  PIC X(2).                                    
006900*                                 MFS ATTRIBUTFÄLT                        
007000     03 MOD-IDAVTAL-IN       PIC Z(11)9.                                  
007100*                                 AVTALSIDENTITET  (PPPBBBBBSSS)          
007200*                                 PPP   = INKÖPARNR (PREFIX)              
007300*                                 BBBBB = BESTÄLLARNR                     
007400*                                 SSS   = SUFFIX                          
007500     03 MOD-IDLEVNR-AVT-IN-ATTR                                           
007600                             PIC X(2).                                    
007700*                                 MFS ATTRIBUTFÄLT                        
007800     03 MOD-IDLEVNR-AVT-IN   PIC X(5).                                    
007900*                                 LEVERANTÖRNUMMER                        
008000     03 MOD-IDLEVNR-SHIP-IN-ATTR                                          
008100                             PIC X(2).                                    
008200*                                 MFS ATTRIBUTFÄLT                        
008300     03 MOD-IDLEVNR-SHIP-IN  PIC X(5).                                    
008400*                                 SKEPPANDE LEVERANTÖR                    
008500     03 MOD-TIAVTAL-IN-ATTR  PIC X(2).                                    
008600*                                 MFS ATTRIBUTFÄLT                        
008700     03 MOD-TIAVTAL-IN       PIC 9(6).                                    
008800*                                 AVTALSDATUM  (ÅÅMMDD)                   
008900     03 MOD-KDCMD-NC-IN-ATTR PIC X(2).                                    
009000*                                 MFS ATTRIBUTFÄLT                        
009100     03 MOD-KDCMD-NC-IN      PIC X.                                       
009200*                                 RAD-UPPDATERINGSKOMMANDO                
009300*                                  BLANK  = INGENTING                     
009400*                                  D , B  = DELETE                        
009500*                                  R , Ä  = REPLACE                       
009600*                                  I,N,A  = INSERT                        
009700*                                  S , V  = SELECT                        
009800*                                  P , P  = PRINT                         
009900*                                  C , K  = COPY                          
010000     03 MOD-KDCMD-C-IN-ATTR  PIC X(2).                                    
010100*                                 MFS ATTRIBUTFÄLT                        
010200     03 MOD-KDCMD-C-IN       PIC X.                                       
010300*                                 RAD-UPPDATERINGSKOMMANDO                
010400*                                  BLANK  = INGENTING                     
010500*                                  D , B  = DELETE                        
010600*                                  R , Ä  = REPLACE                       
010700*                                  I,N,A  = INSERT                        
010800*                                  S , V  = SELECT                        
010900*                                  P , P  = PRINT                         
011000*                                  C , K  = COPY                          
011100     03 MOD-TEMFSINF         PIC X(55).                                   
011200*                                 INFORMATIONSMEDDELANDE                  
011300*** END OF VILMAII-COPY LENGTH= 408 BYTES                                 
