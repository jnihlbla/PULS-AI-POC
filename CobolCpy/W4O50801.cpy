000100 01  MOD-W4O50801.                                                        
000200*                                 MODCOPYTEXT TILL W40508.                
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-MESSAGE-RAD1     PIC X(40).                                   
000600*                                 MEDDELANDEFÄLT PÅ RAD 1                 
000700     03 MOD-IDDISTR-IN       PIC X(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 MOD-IDORDNR7-IN      PIC X(7).                                    
001200*                                 ORDERNUMMER                             
001300     03 MOD-IDPRODNR-IN      PIC X(7).                                    
001400*                                 PRODUKTIONSNUMMER                       
001500     03 MOD-IDKOLLI-IN       PIC X(5).                                    
001600*                                 KOLLINUMMER                             
001700     03 MOD-IDARTNR-IN       PIC X(9).                                    
001800*                                 ARTIKELNUMMER                           
001900     03 MOD-IDDC-IN          PIC X(2).                                    
002000*                                 IDENTIFIERARE LAGER                     
002100     03 MOD-IDDISTR-UT       PIC X(4).                                    
002200*                                 DISTRIKTNUMMER                          
002300     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
002400*                                 KUNDNUMMER                              
002500     03 MOD-IDORDNR7-UT      PIC X(7).                                    
002600*                                 ORDERNUMMER                             
002700     03 MOD-IDPRODNR-UT      PIC X(7).                                    
002800*                                 PRODUKTIONSNUMMER                       
002900     03 MOD-IDKOLLI-UT       PIC X(5).                                    
003000*                                 KOLLINUMMER                             
003100     03 MOD-IDARTNR-UT       PIC X(9).                                    
003200*                                 ARTIKELNUMMER                           
003300     03 MOD-IDDC-UT          PIC X(2).                                    
003400*                                 IDENTIFIERARE LAGER                     
003500     03 MOD-IDKOLLI-ENTER    PIC 9(5).                                    
003600*                                 KOLLINUMMER                             
003700     03 MOD-IDPRODNR-ENTER   PIC 9(7).                                    
003800*                                 PRODUKTIONSNUMMER                       
003900     03 MOD-IDKOLLI-NEXT     PIC 9(5).                                    
004000*                                 KOLLINUMMER                             
004100     03 MOD-IDPRODNR-NEXT    PIC 9(7).                                    
004200*                                 PRODUKTIONSNUMMER                       
004300     03 MOD-TEDDI            PIC X(11).                                   
004400*                                 TEXTFÄLT DDI                            
004500     03 MOD-TETEXT10         PIC X(10).                                   
004600     03 MOD-IDSHIPM          PIC Z(7).                                    
004700*                                 SKEPPNINGSNUMMER                        
004800     03 MOD-HUVUD.                                                        
004900*                                 RAD INNEHÅLLANDE ORDERHUVUDINFO         
005000        05 MOD-KVKOLLI       PIC Z(3)9.                                   
005100*                                 ANTAL KOLLI                             
005200        05 FILLER            PIC X(3).                                    
005300        05 MOD-KVKOLLI-FAKT  PIC Z(3)9.                                   
005400*                                 ANTAL FAKTURERADE KOLLIN                
005500        05 FILLER            PIC X(5).                                    
005600        05 MOD-KVKOLLI-LAST  PIC Z(3)9.                                   
005700*                                 ANTAL LASTNINGSRAPPORTERADE             
005800*                                 KOLLIN                                  
005900        05 FILLER            PIC X(2).                                    
006000        05 MOD-KVORDRAD-TOT  PIC Z(4)9.                                   
006100*                                 ANTAL ORDERRADER                        
006200        05 FILLER            PIC X(2).                                    
006300        05 MOD-VKORDBTO      PIC Z(5)9.9.                                 
006400*                                 ORDERVIKT BRUTTO (KG)                   
006500        05 FILLER            PIC X(2).                                    
006600        05 MOD-VLORDBTO      PIC Z(3)9.9(3).                              
006700*                                 ORDERVOLYM BRUTTO (M3)                  
006800        05 FILLER            PIC X.                                       
006900        05 MOD-SUORDV        PIC Z(8)9.9(2).                              
007000*                                 SUMMA ORDERVÄRDE                        
007100        05 MOD-TEASTRIX      PIC X.                                       
007200*                                 ASTERISK                                
007300     03 MOD-VARHEAD          PIC X(15).                                   
007400     03 MOD-VARHEAD2         PIC X(14).                                   
007500     03 MOD-VARHEAD3         PIC X(8).                                    
007600     03 MOD-RAD              OCCURS 11 TIMES.                             
007700*                                 TABELL INNEHÅLLANDE RADER.              
007800        05 MOD-IDKOLLI       PIC Z(4)9.                                   
007900*                                 KOLLINUMMER                             
008000        05 MOD-ADKOLLI-GRP.                                               
008100*                                 ADKOLLI-GRP.                            
008200           07 FILLER         PIC X.                                       
008300           07 MOD-ADFLGEO    PIC X(3).                                    
008400*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
008500           07 MOD-ADFLOMR    PIC X(5).                                    
008600           07 MOD-ADRUTNIV   PIC Z(2)9.                                   
008700*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
008800           07 FILLER         PIC X.                                       
008900           07 MOD-ADVMODUL   PIC Z(3).                                    
009000*                                 VÄNSTER-MODUL                           
009100        05 FILLER            PIC X.                                       
009200        05 MOD-TIPACKN       PIC 9(6).                                    
009300*                                 PACKNINGSDATUM         (ÅÅMMDD)         
009400        05 FILLER            PIC X.                                       
009500        05 MOD-TIFAKT        PIC 9(6).                                    
009600*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
009700        05 FILLER            PIC X(2).                                    
009800        05 MOD-TILASTN       PIC 9(6).                                    
009900*                                 LASTNINGSDATUM         (ÅÅMMDD)         
010000        05 FILLER            PIC X(2).                                    
010100        05 MOD-KVORDRAD      PIC Z(4)9.                                   
010200*                                 ANTAL ORDERRADER                        
010300        05 FILLER            PIC X.                                       
010400        05 MOD-VKORDBTO-KOLLI                                             
010500                             PIC Z(5)9.9.                                 
010600*                                 ORDERVIKT BRUTTO PER KOLLI              
010700        05 FILLER            PIC X.                                       
010800        05 MOD-VLORDBTO-KOLLI                                             
010900                             PIC Z(3)9.9(3).                              
011000*                                 ORDERVOLYM BRUTTO KOLLI                 
011100        05 MOD-KDKOLLI REDEFINES MOD-VLORDBTO-KOLLI                       
011200                             PIC X(8).                                    
011300*                                 KOLLIKOD                                
011400        05 FILLER            PIC X.                                       
011500        05 MOD-TETEXTX2      PIC X(2).                                    
011600        05 FILLER            PIC X(2).                                    
011700        05 MOD-IDPLOCK       PIC Z(5)9.                                   
011800*                                 PLOCKARE                                
011900     03 MOD-MESSAGE-RAD23    PIC X(61).                                   
012000*                                 MEDDELANDEFÄLT PÅ RAD 23                
012100*** END OF VILMAII-COPY LENGTH= 1204 BYTES                                
