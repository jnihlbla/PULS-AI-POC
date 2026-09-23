000100 01  MOD-W4O71201.                                                        
000200*                                 MOD-COPYTEXT FÖR W4071200               
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
001500     03 MOD-IDRAPPNR-IN      PIC X(7).                                    
001600*                                 RAPPORT NUMMER                          
001700     03 MOD-IDRAPPNR-UT      PIC X(7).                                    
001800*                                 RAPPORT NUMMER                          
001900     03 MOD-IDARTNR-IN       PIC X(8).                                    
002000*                                 ARTIKELNUMMER                           
002100     03 MOD-IDARTNR-UT       PIC X(8).                                    
002200*                                 ARTIKELNUMMER                           
002300     03 MOD-IDRADNR-IN       PIC X(4).                                    
002400*                                 RADNUMMER                               
002500     03 MOD-IDRADNR-UT       PIC X(4).                                    
002600*                                 RADNUMMER                               
002700     03 MOD-IDARTNR-ENTER    PIC 9(8).                                    
002800*                                 ARTIKELNUMMER                           
002900     03 MOD-IDRADNR-ENTER    PIC 9(5).                                    
003000*                                 RADNUMMER                               
003100     03 MOD-IDARTNR-NEXT     PIC 9(8).                                    
003200*                                 ARTIKELNUMMER                           
003300     03 MOD-IDRADNR-NEXT     PIC 9(5).                                    
003400*                                 RADNUMMER                               
003500     03 MOD-TILEVANM         PIC 9(6).                                    
003600*                                 DATUM LEVERANSANMÄRKNING                
003700     03 MOD-KDLEVANM         PIC X.                                       
003800*                                 STATUS LEVERANSANMÄRKNING               
003900     03 MOD-KDVALISO         PIC X(3).                                    
004000*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
004100     03 MOD-INFO-RAD         OCCURS 12 TIMES.                             
004200*                                 RADINFORMATION                          
004300        05 MOD-IDARTNR-ATTRIBUT                                           
004400                             PIC X(2).                                    
004500*                                 MFS ATTRIBUTFÄLT                        
004600        05 MOD-IDARTNR       PIC Z(7)9.                                   
004700*                                 ARTIKELNUMMER                           
004800        05 MOD-IDRADNR       PIC Z(3)9.                                   
004900*                                 RADNUMMER                               
005000        05 MOD-KDANMORS-ATTRIBUT                                          
005100                             PIC X(2).                                    
005200*                                 MFS ATTRIBUTFÄLT                        
005300        05 MOD-KDANMORS      PIC 9(2).                                    
005400*                                 ORSAK TILL LEVERAN KDANMORS-002         
005500        05 MOD-RAD-ATTRIBUT  PIC X(2).                                    
005600*                                 MFS ATTRIBUTFÄLT                        
005700        05 MOD-IDORDNR       PIC Z(4)9.                                   
005800*                                 ORDERNUMMER UTGÅR PD90                  
005900        05 FILLER            PIC X.                                       
006000        05 MOD-IDKOLLI       PIC Z(4)9.                                   
006100*                                 KOLLINUMMER                             
006200        05 FILLER            PIC X.                                       
006300        05 MOD-IDDC          PIC X(2).                                    
006400*                                 IDENTIFIERARE LAGER                     
006500        05 MOD-KVLEVANM      PIC Z(5)9.                                   
006600*                                 LEVERANSANMÄRKNINGSANTAL                
006700        05 FILLER            PIC X(2).                                    
006800        05 MOD-KDEMBLEV      PIC 9.                                       
006900*                                 EMBALLAGEKOD PÅ LEVERANSANMÄRKN         
007000        05 FILLER            PIC X.                                       
007100        05 MOD-PRARTBTO      PIC Z(6)9.9(2).                              
007200*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
007300        05 FILLER            PIC X.                                       
007400        05 MOD-KDFAKTYP      PIC X.                                       
007500*                                 FAKTURATYP                              
007600        05 MOD-STRECK        PIC X.                                       
007700        05 MOD-IDFAKT        PIC Z(7).                                    
007800*                                 FAKTURANUMMER                           
007900        05 FILLER            PIC X.                                       
008000        05 MOD-TIFAKT        PIC 9(6).                                    
008100*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
008200        05 FILLER            PIC X.                                       
008300        05 MOD-FLDIRLEV      PIC X.                                       
008400*                                 DIREKTLEVERANS ?                        
008500        05 FILLER            PIC X.                                       
008600        05 MOD-FLTEXT        PIC X.                                       
008700*                                 FINNS TEXTINFORMATION ?                 
008800        05 MOD-KDKREBEH-ATTRIBUT                                          
008900                             PIC X(2).                                    
009000*                                 MFS ATTRIBUTFÄLT                        
009100        05 MOD-KDKREBEH      PIC X(3).                                    
009200*                                 BEHANDLINGSSTATUS                       
009300     03 MOD-RAD18-IDARTNR-ATTRIBUT                                        
009400                             PIC X(2).                                    
009500*                                 MFS ATTRIBUTFÄLT                        
009600     03 MOD-RAD18-IDARTNR    PIC Z(7)9.                                   
009700*                                 ARTIKELNUMMER                           
009800     03 MOD-RAD18-IDRADNR-ATTRIBUT                                        
009900                             PIC X(2).                                    
010000*                                 MFS ATTRIBUTFÄLT                        
010100     03 MOD-RAD18-IDRADNR    PIC Z(3)9.                                   
010200*                                 RADNUMMER                               
010300     03 MOD-RAD18-KDANMORS-ATTRIBUT                                       
010400                             PIC X(2).                                    
010500*                                 MFS ATTRIBUTFÄLT                        
010600     03 MOD-RAD18-KDANMORS   PIC 9(2).                                    
010700*                                 ORSAK TILL LEVERAN KDANMORS-002         
010800     03 MOD-RAD18-KVLEVANM-ATTRIBUT                                       
010900                             PIC X(2).                                    
011000*                                 MFS ATTRIBUTFÄLT                        
011100     03 MOD-RAD18-KVLEVANM   PIC Z(5)9.                                   
011200*                                 LEVERANSANMÄRKNINGSANTAL                
011300     03 MOD-RAD18-PRARTBTO-ATTRIBUT                                       
011400                             PIC X(2).                                    
011500*                                 MFS ATTRIBUTFÄLT                        
011600     03 MOD-RAD18-PRARTBTO   PIC X(10).                                   
011700*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
011800     03 MOD-RAD18-FLDIRLEV-ATTRIBUT                                       
011900                             PIC X(2).                                    
012000*                                 MFS ATTRIBUTFÄLT                        
012100     03 MOD-RAD18-FLDIRLEV   PIC X.                                       
012200*                                 DIREKTLEVERANS ?                        
012300     03 MOD-TEMFSINF         PIC X(55).                                   
012400*                                 INFORMATIONSMEDDELANDE                  
012500*** END OF VILMAII-COPY LENGTH= 1196 BYTES                                
