000100 01  MOD-W4O50201.                                                        
000200*                                 MODCOPYTEXT TILL W40502.                
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDISTR-IN       PIC X(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 MOD-IDDISTR-UT       PIC X(4).                                    
001000*                                 DISTRIKTNUMMER                          
001100     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
001200*                                 KUNDNUMMER                              
001300     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001400*                                 KUNDNUMMER                              
001500     03 MOD-IDKUNDRF-IN      PIC X(7).                                    
001600*                                 ORDERNUMMER                             
001700     03 MOD-IDKUNDRF-UT      PIC X(7).                                    
001800*                                 ORDERNUMMER                             
001900     03 MOD-IDARTNR-IN       PIC X(9).                                    
002000*                                 ARTIKELNUMMER                           
002100     03 MOD-IDARTNR-UT       PIC X(9).                                    
002200*                                 ARTIKELNUMMER                           
002300     03 MOD-IDKOLLI-IN       PIC X(5).                                    
002400*                                 KOLLINUMMER                             
002500     03 MOD-IDKOLLI-UT       PIC X(5).                                    
002600*                                 KOLLINUMMER                             
002700     03 MOD-IDPRODNR-IN      PIC X(7).                                    
002800*                                 PRODUKTIONSNUMMER                       
002900     03 MOD-IDPRODNR-UT      PIC X(7).                                    
003000*                                 PRODUKTIONSNUMMER                       
003100     03 MOD-IDDC-IN          PIC X(2).                                    
003200*                                 IDENTIFIERARE LAGER                     
003300     03 MOD-IDDC-UT          PIC X(2).                                    
003400*                                 IDENTIFIERARE LAGER                     
003500     03 MOD-IDORDER-ENTER    PIC 9(7).                                    
003600*                                 VOLVO PARTS ORDERNUMMER                 
003700     03 MOD-IDORDER-NEXT     PIC 9(7).                                    
003800*                                 VOLVO PARTS ORDERNUMMER                 
003900     03 MOD-IDARTNR-ENTER    PIC 9(9).                                    
004000*                                 ARTIKELNUMMER                           
004100     03 MOD-IDARTNR-NEXT     PIC 9(9).                                    
004200*                                 ARTIKELNUMMER                           
004300     03 MOD-IDPURAD-ENTER    PIC 9(5).                                    
004400*                                 RADNUMMER                               
004500     03 MOD-IDPURAD-NEXT     PIC 9(5).                                    
004600*                                 RADNUMMER                               
004700     03 MOD-IDPRODNR-ENTER   PIC 9(7).                                    
004800*                                 PRODUKTIONSNUMMER                       
004900     03 MOD-IDPRODNR-NEXT    PIC 9(7).                                    
005000*                                 PRODUKTIONSNUMMER                       
005100     03 MOD-IDDC-ENTER       PIC X(2).                                    
005200*                                 IDENTIFIERARE LAGER                     
005300     03 MOD-IDDC-NEXT        PIC X(2).                                    
005400*                                 IDENTIFIERARE LAGER                     
005500     03 MOD-TEDDI            PIC X(11).                                   
005600*                                 TEXTFÄLT DDI                            
005700     03 MOD-VARHEAD          PIC X(10).                                   
005800     03 MOD-IDARTNR          OCCURS 13 TIMES                              
005900                             PIC Z(8)9.                                   
006000*                                 ARTIKELNUMMER                           
006100     03 MOD-KVBEART-Q        OCCURS 13 TIMES                              
006200                             PIC Z(6)9.                                   
006300     03 MOD-KVAVBART         OCCURS 13 TIMES                              
006400                             PIC Z(6)9.                                   
006500     03 MOD-KVLEVART         OCCURS 13 TIMES                              
006600                             PIC Z(6)9.                                   
006700     03 MOD-KDAVVIK          OCCURS 13 TIMES                              
006800                             PIC X.                                       
006900*                                 AVVIKELSE-KOD                           
007000     03 MOD-PRARTNTO-GRP.                                                 
007100*                                 NETTOPRIS (SVENSKA)                     
007200        05 MOD-PRARTNTO-FILLER.                                           
007300           07 MOD-PRARTNTO   OCCURS 13 TIMES                              
007400                             PIC Z(6)9.9(2).                              
007500*                                 ARTIKELPRIS NETTO                       
007600        05 MOD-PRARTNTO-FIX REDEFINES MOD-PRARTNTO-FILLER                 
007700                             OCCURS 13 TIMES                              
007800                             PIC X(10).                                   
007900*                                 ARTIKELPRIS NETTO                       
008000     03 MOD-KDKOLSTA         OCCURS 13 TIMES                              
008100                             PIC X(2).                                    
008200*                                 KOLLI-STATUS       KDKOLSTA-002         
008300     03 MOD-IDKOLLI          OCCURS 13 TIMES                              
008400                             PIC Z(4)9.                                   
008500*                                 KOLLINUMMER                             
008600     03 MOD-KDFARLIG         OCCURS 13 TIMES                              
008700                             PIC X.                                       
008800*                                 KOD FÖR FARLIGT GODS                    
008900     03 MOD-IDFAKT           OCCURS 13 TIMES                              
009000                             PIC Z(6)9.                                   
009100*                                 FAKTURANUMMER                           
009200     03 MOD-IDLEVNR          OCCURS 13 TIMES                              
009300                             PIC X(5).                                    
009400*                                 LEVERANTÖRNUMMER                        
009500     03 MOD-IDKUNDRF-URS     OCCURS 13 TIMES                              
009600                             PIC 9(7).                                    
009700*                                 ORDERNUMMER                             
009800     03 MOD-ADLAGOMR-ENTER   PIC Z(2)9.                                   
009900*                                 LAGEROMRÅDE                             
010000     03 MOD-ADLAGOMR-NEXT    PIC Z(2)9.                                   
010100*                                 LAGEROMRÅDE                             
010200     03 MOD-ADGANG-ENTER     PIC Z(2)9.                                   
010300*                                 GÅNG                                    
010400     03 MOD-ADGANG-NEXT      PIC Z(2)9.                                   
010500*                                 GÅNG                                    
010600     03 MOD-ADPLATS-ENTER    PIC 9(5).                                    
010700*                                 LAGERPLATSNUMMER                        
010800     03 MOD-ADPLATS-NEXT     PIC 9(5).                                    
010900*                                 LAGERPLATSNUMMER                        
011000     03 MOD-IDLOPNR-ENTER    PIC 9(3).                                    
011100*                                 LÖPNUMMER                               
011200     03 MOD-IDLOPNR-NEXT     PIC 9(3).                                    
011300*                                 LÖPNUMMER                               
011400     03 MOD-IDPLKLST-ENTER   PIC Z(2)9.                                   
011500*                                 PLOCKLISTNUMMER                         
011600     03 MOD-IDPLKLST-NEXT    PIC Z(2)9.                                   
011700*                                 PLOCKLISTNUMMER                         
011800     03 MOD-IDKOLLI-ENTER    PIC 9(5).                                    
011900*                                 KOLLINUMMER                             
012000     03 MOD-IDKOLLI-NEXT     PIC 9(5).                                    
012100*                                 KOLLINUMMER                             
012200     03 MOD-TEMFSINF         PIC X(55).                                   
012300*                                 INFORMATIONSMEDDELANDE                  
012400*** END OF VILMAII-COPY LENGTH= 1188 BYTES                                
