000100 01  MOD-W2O13201.                                                        
000200*                                 MOD-COPYTEXT FÖR W2013200               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(2).                                    
000800*                                 MFS BEHANDLING AV INPUTFÄLT             
000900     03 MOD-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-C2FAELT-SW       PIC X.                                       
001200     03 MOD-BEART            PIC X(25).                                   
001300*                                 ARTIKELBENÄMNING                        
001400     03 MOD-REDIRLEV-C1-ATTR PIC X(2).                                    
001500*                                 MFS ATTRIBUTFÄLT                        
001600     03 MOD-REDIRLEV-C1      PIC 9.9(2).                                  
001700*                                 DIREKTLEVERANSANDEL                     
001800     03 MOD-FLFSP-ATTR       PIC X(2).                                    
001900*                                 MFS ATTRIBUTFÄLT                        
002000     03 MOD-FLFSP            PIC X.                                       
002100*                                 FÖRDELNINGSSPÄRR                        
002200     03 MOD-KVSLUTKP-ATTR    PIC X(2).                                    
002300*                                 MFS ATTRIBUTFÄLT                        
002400     03 MOD-KVSLUTKP         PIC Z(6)9.                                   
002500*                                 SLUTKÖPSSALDO                           
002600     03 MOD-KDKSP-ATTR-UT    PIC X(2).                                    
002700*                                 MFS ATTRIBUTFÄLT                        
002800     03 MOD-KDKSP-UT         PIC X(2).                                    
002900*                                 MFS BEHANDLING AV INPUTFÄLT             
003000     03 MOD-REDIRLEV-C1-IN-ATTR                                           
003100                             PIC X(2).                                    
003200*                                 MFS ATTRIBUTFÄLT                        
003300     03 MOD-REDIRLEV-C1-IN   PIC X(2).                                    
003400*                                 MFS BEHANDLING AV INPUTFÄLT             
003500     03 MOD-FLFSP-IN-ATTR    PIC X(2).                                    
003600*                                 MFS ATTRIBUTFÄLT                        
003700     03 MOD-FLFSP-IN         PIC X(2).                                    
003800*                                 MFS BEHANDLING AV INPUTFÄLT             
003900     03 MOD-KVSLUTKP-IN-ATTR PIC X(2).                                    
004000*                                 MFS ATTRIBUTFÄLT                        
004100     03 MOD-KVSLUTKP-IN      PIC X(2).                                    
004200*                                 MFS BEHANDLING AV INPUTFÄLT             
004300     03 MOD-KDKSP-IN-ATTR    PIC X(2).                                    
004400*                                 MFS ATTRIBUTFÄLT                        
004500     03 MOD-KDKSP-IN         PIC X(2).                                    
004600*                                 MFS BEHANDLING AV INPUTFÄLT             
004700     03 MOD-KDAVT-ATTR       PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900     03 MOD-KDAVT            PIC 9.                                       
005000*                                 AVTALSMÄRKNING                          
005100     03 MOD-IDINK-ATTR       PIC X(2).                                    
005200*                                 MFS ATTRIBUTFÄLT                        
005300     03 MOD-IDINK            PIC X(4).                                    
005400*                                 INKÖPARNUMMER                           
005500     03 MOD-TISLUTKP-ATTR    PIC X(2).                                    
005600*                                 MFS ATTRIBUTFÄLT                        
005700     03 MOD-TISLUTKP         PIC 9(5).                                    
005800*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
005900     03 MOD-FLNYBER-ATTR     PIC X(2).                                    
006000*                                 MFS ATTRIBUTFÄLT                        
006100     03 MOD-FLNYBER          PIC X.                                       
006200*                                 FLAGGA BERÄKN HEMTAGN NYTT SÄTT         
006300     03 MOD-KDAVT-IN-ATTR    PIC X(2).                                    
006400*                                 MFS ATTRIBUTFÄLT                        
006500     03 MOD-KDAVT-IN         PIC X(2).                                    
006600*                                 MFS BEHANDLING AV INPUTFÄLT             
006700     03 MOD-IDINK-IN-ATTR    PIC X(2).                                    
006800*                                 MFS ATTRIBUTFÄLT                        
006900     03 MOD-IDINK-IN         PIC X(2).                                    
007000*                                 MFS BEHANDLING AV INPUTFÄLT             
007100     03 MOD-TISLUTKP-IN-ATTR PIC X(2).                                    
007200*                                 MFS ATTRIBUTFÄLT                        
007300     03 MOD-TISLUTKP-IN      PIC X(2).                                    
007400*                                 MFS BEHANDLING AV INPUTFÄLT             
007500     03 MOD-FLNYBER-IN-ATTR  PIC X(2).                                    
007600*                                 MFS ATTRIBUTFÄLT                        
007700     03 MOD-FLNYBER-IN       PIC X(2).                                    
007800*                                 MFS BEHANDLING AV INPUTFÄLT             
007900     03 MOD-KDSOP-ATTR       PIC X(2).                                    
008000*                                 MFS ATTRIBUTFÄLT                        
008100     03 MOD-KDSOP            PIC X.                                       
008200*                                 VISAR NÄR START DAT ART GÄLLER          
008300     03 MOD-KDSOP-IN-ATTR    PIC X(2).                                    
008400*                                 MFS ATTRIBUTFÄLT                        
008500     03 MOD-KDSOP-IN         PIC X.                                       
008600*                                 VISAR NÄR START DAT ART GÄLLER          
008700     03 MOD-KVEOP-ATTR       PIC X(2).                                    
008800*                                 MFS ATTRIBUTFÄLT                        
008900     03 MOD-KVEOP            PIC Z9.                                      
009000*                                 ANTAL ÅR KVAR FÖR ART EFTER EOP         
009100     03 MOD-KVEOP-IN-ATTR    PIC X(2).                                    
009200*                                 MFS ATTRIBUTFÄLT                        
009300     03 MOD-KVEOP-IN         PIC Z9.                                      
009400*                                 ANTAL ÅR KVAR FÖR ART EFTER EOP         
009500     03 MOD-FLJIT-ATTR       PIC X(2).                                    
009600*                                 MFS ATTRIBUTFÄLT                        
009700     03 MOD-FLJIT            PIC X.                                       
009800*                                 JUST-IN-TIME FLAGGA                     
009900     03 MOD-FLJIT-IN-ATTR    PIC X(2).                                    
010000*                                 MFS ATTRIBUTFÄLT                        
010100     03 MOD-FLJIT-IN         PIC X.                                       
010200*                                 JUST-IN-TIME FLAGGA                     
010300     03 MOD-FLBSNES-ATTR     PIC X(2).                                    
010400*                                 MFS ATTRIBUTFÄLT                        
010500     03 MOD-FLBSNES          PIC X.                                       
010600*                                 ARTIKEL MED MER AFFÄRSVÄRDE             
010700     03 MOD-FLBSNES-IN-ATTR  PIC X(2).                                    
010800*                                 MFS ATTRIBUTFÄLT                        
010900     03 MOD-FLBSNES-IN       PIC X.                                       
011000*                                 ARTIKEL MED MER AFFÄRSVÄRDE             
011100     03 MOD-FLBRAND-ATTR     PIC X(2).                                    
011200*                                 MFS ATTRIBUTFÄLT                        
011300     03 MOD-FLBRAND          PIC X.                                       
011400*                                 ARTIKEL MED VARUMÄRKESBILD              
011500     03 MOD-FLBRAND-IN-ATTR  PIC X(2).                                    
011600*                                 MFS ATTRIBUTFÄLT                        
011700     03 MOD-FLBRAND-IN       PIC X.                                       
011800*                                 ARTIKEL MED VARUMÄRKESBILD              
011900     03 MOD-TISTODAT-LARM-ATTR                                            
012000                             PIC X(2).                                    
012100*                                 MFS ATTRIBUTFÄLT                        
012200     03 MOD-TISTODAT-LARM    PIC 9(6).                                    
012300*                                 STOPPDATUM FÖR LARM-223                 
012400     03 MOD-TISTODAT-LARM-IN-ATTR                                         
012500                             PIC X(2).                                    
012600*                                 MFS ATTRIBUTFÄLT                        
012700     03 MOD-TISTODAT-LARM-IN PIC X(2).                                    
012800*                                 MFS BEHANDLING AV INPUTFÄLT             
012900     03 MOD-KVKP-ATTR        PIC X(2).                                    
013000*                                 MFS ATTRIBUTFÄLT                        
013100     03 MOD-KVKP             PIC Z(6)9.                                   
013200*                                 KÖPPUNKT                                
013300     03 MOD-FLMANKP-ATTR     PIC X(2).                                    
013400*                                 MFS ATTRIBUTFÄLT                        
013500     03 MOD-FLMANKP          PIC X.                                       
013600*                                 MANUELL FRAMTAGEN KÖPPUNKT ?            
013700     03 MOD-ADINPORT-ATTR    PIC X(2).                                    
013800*                                 MFS ATTRIBUTFÄLT                        
013900     03 MOD-ADINPORT         PIC X(8).                                    
014000*                                 AVLASTNINGSPORT                         
014100     03 MOD-KVKP-IN-ATTR     PIC X(2).                                    
014200*                                 MFS ATTRIBUTFÄLT                        
014300     03 MOD-KVKP-IN          PIC X(2).                                    
014400*                                 MFS BEHANDLING AV INPUTFÄLT             
014500     03 MOD-FLMANKP-IN-ATTR  PIC X(2).                                    
014600*                                 MFS ATTRIBUTFÄLT                        
014700     03 MOD-FLMANKP-IN       PIC X(2).                                    
014800*                                 MFS BEHANDLING AV INPUTFÄLT             
014900     03 MOD-TIFINLV-ATTR     PIC X(2).                                    
015000*                                 MFS ATTRIBUTFÄLT                        
015100     03 MOD-TIFINLV          PIC Z(4)9.                                   
015200*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
015300     03 MOD-TIURPROD-ATTR    PIC X(2).                                    
015400*                                 MFS ATTRIBUTFÄLT                        
015500     03 MOD-TIURPROD         PIC 9(4).                                    
015600*                                 DATUM UTGÅTT UR PROD   (ÅÅVV)           
015700     03 MOD-FLRELSP-ATTR     PIC X(2).                                    
015800*                                 MFS ATTRIBUTFÄLT                        
015900     03 MOD-FLRELSP          PIC X.                                       
016000*                                 RELEASEBLOCKAD ARTIKEL .                
016100     03 MOD-KVVECKOR-LVAR-UT PIC X(4).                                    
016200*                                 VARIANS I LEDTIDEN                      
016300     03 MOD-KVVECKOR-LVAR-IN-ATTR                                         
016400                             PIC X(2).                                    
016500*                                 MFS ATTRIBUTFÄLT                        
016600     03 MOD-KVVECKOR-LVAR-IN PIC X(4).                                    
016700*                                 VARIANS I LEDTIDEN                      
016800     03 MOD-TIFINLV-IN-ATTR  PIC X(2).                                    
016900*                                 MFS ATTRIBUTFÄLT                        
017000     03 MOD-TIFINLV-IN       PIC X(2).                                    
017100*                                 MFS BEHANDLING AV INPUTFÄLT             
017200     03 MOD-TIURPROD-IN-ATTR PIC X(2).                                    
017300*                                 MFS ATTRIBUTFÄLT                        
017400     03 MOD-TIURPROD-IN      PIC X(2).                                    
017500*                                 MFS BEHANDLING AV INPUTFÄLT             
017600     03 MOD-FLRELSP-IN-ATTR  PIC X(2).                                    
017700*                                 MFS ATTRIBUTFÄLT                        
017800     03 MOD-FLRELSP-IN       PIC X(2).                                    
017900*                                 MFS BEHANDLING AV INPUTFÄLT             
018000     03 MOD-TILEVDAGAR       OCCURS 5 TIMES.                              
018100*                                                                         
018200        05 MOD-DAG-POS-ATTR  PIC X(2).                                    
018300*                                 MFS ATTRIBUTFÄLT                        
018400        05 MOD-DAG-POS       PIC X(2).                                    
018500     03 MOD-SPAR-TILEVDAGAR.                                              
018600*                                                                         
018700        05 MOD-SPAR-DAG      OCCURS 5 TIMES                               
018800                             PIC X(2).                                    
018900     03 MOD-TEARTNOT1-IN-ATTR                                             
019000                             PIC X(2).                                    
019100*                                 MFS BEHANDLING AV INPUTFÄLT             
019200     03 MOD-TEARTNOT1-IN     PIC X(40).                                   
019300*                                 ARTIKEL NOTERING                        
019400     03 MOD-TEARTNOT2-IN-ATTR                                             
019500                             PIC X(2).                                    
019600*                                 MFS BEHANDLING AV INPUTFÄLT             
019700     03 MOD-TEARTNOT2-IN     PIC X(40).                                   
019800*                                 ARTIKEL NOTERING                        
019900     03 MOD-TEMFSINF         PIC X(55).                                   
020000*                                 INFORMATIONSMEDDELANDE                  
020100*** END OF VILMAII-COPY LENGTH= 435 BYTES                                 
