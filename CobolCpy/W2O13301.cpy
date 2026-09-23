000100 01  MOD-W2O13301.                                                        
000200*                                 MOD-COPYTEXT FÖR W2013300               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(2).                                    
000800*                                 MFS BEHANDLING AV INPUTFÄLT             
000900     03 MOD-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-IDANSK-FOM       PIC X(3).                                    
001200*                                 ANSKAFFARNUMMER                         
001300     03 MOD-IDANSK-TOM       PIC X(3).                                    
001400*                                 ANSKAFFARNUMMER                         
001500     03 MOD-IDPROJ-VALT      PIC X(4).                                    
001600*                                 PARTS PROJEKTIDENTITET                  
001700     03 MOD-IDANSK-MIN       PIC X(3).                                    
001800*                                 ANSKAFFARNUMMER                         
001900     03 MOD-IDPROJ-MIN       PIC X(4).                                    
002000*                                 PARTS PROJEKTIDENTITET                  
002100     03 MOD-FLPISK-MIN       PIC X.                                       
002200*                                 PISK ARTIKEL                            
002300     03 MOD-TIFINLEV-MIN     PIC 9(6).                                    
002400*                                 PUBLICERINGSDATUM  (AAMMDD)             
002500     03 MOD-IDAO-MIN         PIC X(10).                                   
002600*                                 ÄNDRINGSORDERNUMMER                     
002700     03 MOD-BEART            PIC X(25).                                   
002800*                                 ARTIKELBENÄMNING                        
002900     03 MOD-TIFINLV          PIC 9(5).                                    
003000*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
003100     03 MOD-IDBERED          PIC Z9.                                      
003200*                                 BEREDARENUMMER                          
003300     03 MOD-TEORSAK          PIC X(50).                                   
003400*                                 INFO OM SLAG AV ÅTGÄRD                  
003500     03 MOD-IDAO             PIC X(10).                                   
003600*                                 ÄNDRINGSORDERNUMMER                     
003700     03 MOD-IDKAT-1          PIC X(5).                                    
003800*                                 KATALOGBETECKNING                       
003900     03 MOD-IDKAT-2          PIC X(5).                                    
004000*                                 KATALOGBETECKNING                       
004100     03 MOD-IDKAT-3          PIC X(5).                                    
004200*                                 KATALOGBETECKNING                       
004300     03 MOD-IDPROENH         PIC X(8).                                    
004400*                                 PRODUKTIONSENHET                        
004500     03 MOD-IDPROJ           PIC X(4).                                    
004600*                                 PARTS PROJEKTIDENTITET                  
004700     03 MOD-VARNOT           PIC X(40).                                   
004800*                                 ARTIKEL NOTERING                        
004900     03 MOD-IDPROJK          PIC X(4).                                    
005000*                                 PROJEKTIDENTITET KONSTRUKTION           
005100     03 MOD-IDARTNR-ERS1     PIC Z(7)9.                                   
005200*                                 ERSATT ARTIKELNUMMER                    
005300     03 MOD-KDERS-1          PIC 9(2).                                    
005400*                                 ERSÄTTNINGSKOD                          
005500     03 MOD-MFL              PIC X(3).                                    
005600     03 MOD-IDARTNR-MOTSV    PIC Z(9).                                    
005700*                                 MOTSVARANDE ARTIKEL                     
005800     03 MOD-FLBYTES          PIC X.                                       
005900*                                 BYTESARTIKEL                            
006000     03 MOD-KVARTAR1         PIC Z(8)9.                                   
006100*                                 ANTAL ARTIKLAR FÖRBRUKNING ÅR 1         
006200     03 MOD-KVARTAR2         PIC Z(8)9.                                   
006300*                                 ANTAL ARTIKLAR FÖRBRUKNING ÅR 2         
006400     03 MOD-KVARTAR3         PIC Z(8)9.                                   
006500*                                 ANTAL ARTIKLAR FÖRBRUKNING ÅR 3         
006600     03 MOD-KVARTVAGN        PIC Z(2)9.                                   
006700*                                 ANTAL ARTIKLAR PER VAGN                 
006800     03 MOD-IDLEVNR          PIC X(5).                                    
006900*                                 LEVERANTÖRNUMMER                        
007000     03 MOD-IDPLANGR-LEV     PIC 9.                                       
007100*                                 PLANERINGSGRUPP HOS LEVERANTÖR          
007200     03 MOD-IDPLANGR-AG      PIC 9.                                       
007300*                                 PLANERINGSGRUPP ANSKAFFARE              
007400     03 MOD-IDANSK           PIC Z(2)9.                                   
007500*                                 ANSKAFFARNUMMER                         
007600     03 MOD-IDMATKTO         PIC X(8).                                    
007700*                                 MATRIALKONTO, ANALYSNUMMER              
007800     03 MOD-IDLEVNR-IN-ATTR  PIC X(2).                                    
007900*                                 MFS ATTRIBUTFÄLT                        
008000     03 MOD-IDLEVNR-IN       PIC X(2).                                    
008100*                                 MFS BEHANDLING AV INPUTFÄLT             
008200     03 MOD-IDPLANGR-LEV-IN-ATTR                                          
008300                             PIC X(2).                                    
008400*                                 MFS ATTRIBUTFÄLT                        
008500     03 MOD-IDPLANGR-LEV-IN  PIC X(2).                                    
008600*                                 MFS BEHANDLING AV INPUTFÄLT             
008700     03 MOD-IDPLANGR-AG-IN-ATTR                                           
008800                             PIC X(2).                                    
008900*                                 MFS ATTRIBUTFÄLT                        
009000     03 MOD-IDPLANGR-AG-IN   PIC X(2).                                    
009100*                                 MFS BEHANDLING AV INPUTFÄLT             
009200     03 MOD-IDANSK-IN-ATTR   PIC X(2).                                    
009300*                                 MFS ATTRIBUTFÄLT                        
009400     03 MOD-IDANSK-IN        PIC X(2).                                    
009500*                                 MFS BEHANDLING AV INPUTFÄLT             
009600     03 MOD-IDANSK-REG       PIC Z(2)9.                                   
009700*                                 REGISTRERAD AV ANSKAFFARENUMMER         
009800     03 MOD-TIANSKREG        PIC 9(6).                                    
009900*                                 REGISTRERINGSDATUM ANSKAFFNING          
010000     03 MOD-PRARTSTD         PIC Z(6)9.9(2).                              
010100*                                 ARTIKELSTANDARDPRIS                     
010200     03 MOD-KDSTAINK         PIC 9.                                       
010300*                                 STATUS PRIS FRÅN INKÖP                  
010400     03 MOD-KDTIPPR          PIC X.                                       
010500*                                 TIPPAT PRIS KOD                         
010600     03 MOD-IDFTG            PIC 9(2).                                    
010700*                                 FÖRETAGSID EKONOM REDOVISNING           
010800     03 MOD-IDLKTO           PIC 9(7).                                    
010900*                                 LAGERKONTO (FFHHHUU)                    
011000     03 MOD-IDLKTO-POS1-7 REDEFINES MOD-IDLKTO.                           
011100*                                                                         
011200        05 MOD-FILLER        PIC X(2).                                    
011300        05 MOD-IDLKTO-POS3-7 PIC X(5).                                    
011400     03 MOD-IDAVD            PIC Z(4)9.                                   
011500*                                 DEN ANSTÄLLDES AVDELNING/               
011600*                                 KOSTNADSSTÄLLE                          
011700     03 MOD-PRARTSTD-IN-ATTR PIC X(2).                                    
011800*                                 MFS ATTRIBUTFÄLT                        
011900     03 MOD-PRARTSTD-IN      PIC X(2).                                    
012000*                                 MFS BEHANDLING AV INPUTFÄLT             
012100     03 MOD-KDTIPPR-IN-ATTR  PIC X(2).                                    
012200*                                 MFS ATTRIBUTFÄLT                        
012300     03 MOD-KDTIPPR-IN       PIC X(2).                                    
012400*                                 MFS BEHANDLING AV INPUTFÄLT             
012500     03 MOD-IDFTG-IN-ATTR    PIC X(2).                                    
012600*                                 MFS ATTRIBUTFÄLT                        
012700     03 MOD-IDFTG-IN         PIC X(2).                                    
012800*                                 MFS BEHANDLING AV INPUTFÄLT             
012900     03 MOD-IDLKTO-IN-ATTR   PIC X(2).                                    
013000*                                 MFS ATTRIBUTFÄLT                        
013100     03 MOD-IDLKTO-IN        PIC X(2).                                    
013200*                                 MFS BEHANDLING AV INPUTFÄLT             
013300     03 MOD-KDSORT           PIC X(2).                                    
013400*                                 SORT-KOD                                
013500     03 MOD-KVBASL-ATTR      PIC X(2).                                    
013600*                                 MFS ATTRIBUTFÄLT                        
013700     03 MOD-KVBASL           PIC Z(6)9.                                   
013800*                                 BASLAGER TOTAL PER ARTIKEL              
013900     03 MOD-KVBASL-KLAR      PIC X.                                       
014000     03 MOD-KVPB-C1-IN-ATTR  PIC X(2).                                    
014100*                                 MFS ATTRIBUTFÄLT                        
014200     03 MOD-KVPB-C1-IN       PIC X(2).                                    
014300*                                 MFS BEHANDLING AV INPUTFÄLT             
014400     03 MOD-KVPB-C1          PIC Z(5)9.9.                                 
014500*                                 SEPARAT PERIODBEHOV                     
014600     03 MOD-TIPBLOCK-IN-ATTR PIC X(2).                                    
014700*                                 MFS ATTRIBUTFÄLT                        
014800     03 MOD-TIPBLOCK-IN      PIC X(6).                                    
014900*                                 FRAMTIDA DATUM FÖR PB-LÅSNING Å         
015000*                                 ÅMMDD                                   
015100     03 MOD-TIPBLOCK         PIC 9(6).                                    
015200*                                 FRAMTIDA DATUM FÖR PB-LÅSNING Å         
015300*                                 ÅMMDD                                   
015400     03 MOD-FLMPB-C1-IN-ATTR PIC X(2).                                    
015500*                                 MFS ATTRIBUTFÄLT                        
015600     03 MOD-FLMPB-C1-IN      PIC X(2).                                    
015700*                                 MFS BEHANDLING AV INPUTFÄLT             
015800     03 MOD-FLMPB-C1         PIC X.                                       
015900*                                 MASKINELLT UPPDAT PERIODBEHOV ?         
016000     03 MOD-KDHF-IN-ATTR     PIC X(2).                                    
016100*                                 MFS ATTRIBUTFÄLT                        
016200     03 MOD-KDHF-IN          PIC X(2).                                    
016300*                                 MFS BEHANDLING AV INPUTFÄLT             
016400     03 MOD-KDHF             PIC 9.                                       
016500*                                 HUVUDFÖRRÅDSMÄRKNING                    
016600     03 MOD-RESLJUST-C1-IN-ATTR                                           
016700                             PIC X(2).                                    
016800*                                 MFS ATTRIBUTFÄLT                        
016900     03 MOD-RESLJUST-C1-IN   PIC 9.9.                                     
017000*                                 SÄKERHETSLAGER-JUSTERINGFAKTOR          
017100     03 MOD-RESLJUST-C1      PIC 9.9.                                     
017200*                                 SÄKERHETSLAGER-JUSTERINGFAKTOR          
017300     03 MOD-KVSLAGER         PIC Z(5)9.                                   
017400*                                 SÄKERHETSLAGER                          
017500     03 MOD-KVMP             PIC Z(6)9.                                   
017600*                                 MAXPUNKT                                
017700     03 MOD-IDANSK-NOT-IN-ATTR                                            
017800                             PIC X(2).                                    
017900*                                 MFS ATTRIBUTFÄLT                        
018000     03 MOD-IDANSK-NOT-IN    PIC X(40).                                   
018100*                                 ARTIKEL NOTERING                        
018200     03 MOD-IDINK-IN-ATTR    PIC X(2).                                    
018300*                                 MFS ATTRIBUTFÄLT                        
018400     03 MOD-IDINK-IN         PIC X(2).                                    
018500*                                 MFS BEHANDLING AV INPUTFÄLT             
018600     03 MOD-IDINK            PIC X(4).                                    
018700*                                 INKÖPARNUMMER                           
018800     03 MOD-KDKOPTYP-IN-ATTR PIC X(2).                                    
018900*                                 MFS ATTRIBUTFÄLT                        
019000     03 MOD-KDKOPTYP-IN      PIC X(2).                                    
019100*                                 MFS BEHANDLING AV INPUTFÄLT             
019200     03 MOD-TILEVBEG-IN-ATTR PIC X(2).                                    
019300*                                 MFS ATTRIBUTFÄLT                        
019400     03 MOD-TILEVBEG-IN      PIC X(2).                                    
019500*                                 MFS BEHANDLING AV INPUTFÄLT             
019600     03 MOD-KVLEVBEG-IN-ATTR PIC X(2).                                    
019700*                                 MFS ATTRIBUTFÄLT                        
019800     03 MOD-KVLEVBEG-IN      PIC X(2).                                    
019900*                                 MFS BEHANDLING AV INPUTFÄLT             
020000     03 MOD-KVPROG           PIC Z(6)9.                                   
020100*                                 ÅRSPROGNOS                              
020200     03 MOD-TEANSINK-IN-ATTR PIC X(2).                                    
020300*                                 MFS ATTRIBUTFÄLT                        
020400     03 MOD-TEANSINK-IN      PIC X(2).                                    
020500*                                 MFS BEHANDLING AV INPUTFÄLT             
020600     03 MOD-KVPROG-IN-ATTR   PIC X(2).                                    
020700*                                 MFS ATTRIBUTFÄLT                        
020800     03 MOD-KVPROG-IN        PIC X(2).                                    
020900*                                 MFS BEHANDLING AV INPUTFÄLT             
021000     03 MOD-FLNYRAPP-IN-ATTR PIC X(2).                                    
021100*                                 MFS ATTRIBUTFÄLT                        
021200     03 MOD-FLNYRAPP-IN      PIC X(2).                                    
021300*                                 MFS BEHANDLING AV INPUTFÄLT             
021400     03 MOD-TEMFSINF         PIC X(55).                                   
021500*                                 INFORMATIONSMEDDELANDE                  
021600*** END OF VILMAII-COPY LENGTH= 595 BYTES                                 
