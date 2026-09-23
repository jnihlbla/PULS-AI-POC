000100 01  MOD-W4O50901.                                                        
000200*                                 MODCOPYTEXT TILL W40509.                
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
001500     03 MOD-IDARTNR-IN       PIC X(9).                                    
001600*                                 ARTIKELNUMMER                           
001700     03 MOD-IDARTNR-UT       PIC X(9).                                    
001800*                                 ARTIKELNUMMER                           
001900     03 MOD-IDDC-IN          PIC X(2).                                    
002000*                                 IDENTIFIERARE LAGER                     
002100     03 MOD-IDDC-UT          PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300     03 MOD-IDORDNR7-IN      PIC X(7).                                    
002400*                                 ORDERNUMMER                             
002500     03 MOD-IDORDNR7-UT      PIC X(7).                                    
002600*                                 ORDERNUMMER                             
002700     03 MOD-IDKOLLI-IN       PIC X(5).                                    
002800*                                 KOLLINUMMER                             
002900     03 MOD-IDKOLLI-UT       PIC X(5).                                    
003000*                                 KOLLINUMMER                             
003100     03 MOD-IDPRODNR-IN      PIC X(7).                                    
003200*                                 PRODUKTIONSNUMMER                       
003300     03 MOD-IDPRODNR-UT      PIC X(7).                                    
003400*                                 PRODUKTIONSNUMMER                       
003500     03 MOD-IDRADNR-ENTER    PIC 9(4).                                    
003600*                                 RADNUMMER                               
003700     03 MOD-IDRADNR-NEXT     PIC 9(4).                                    
003800*                                 RADNUMMER                               
003900     03 MOD-IDKUNDNR-ENTER   PIC 9(7).                                    
004000*                                 KUNDNUMMER                              
004100     03 MOD-IDKUNDNR-NEXT    PIC 9(7).                                    
004200*                                 KUNDNUMMER                              
004300     03 MOD-IDORDNR7-ENTER   PIC 9(7).                                    
004400*                                 ORDERNUMMER                             
004500     03 MOD-IDORDNR7-NEXT    PIC 9(7).                                    
004600*                                 ORDERNUMMER                             
004700     03 MOD-IDORDER-ENTER    PIC 9(7).                                    
004800*                                 VOLVO PARTS ORDERNUMMER                 
004900     03 MOD-IDORDER-NEXT     PIC 9(7).                                    
005000*                                 VOLVO PARTS ORDERNUMMER                 
005100     03 MOD-IDDC-ENTER       PIC X(2).                                    
005200*                                 IDENTIFIERARE LAGER                     
005300     03 MOD-IDDC-NEXT        PIC X(2).                                    
005400*                                 IDENTIFIERARE LAGER                     
005500     03 MOD-ADLAGOMR-ENTER   PIC 9(3).                                    
005600*                                 LAGEROMRÅDE                             
005700     03 MOD-ADLAGOMR-NEXT    PIC 9(3).                                    
005800*                                 LAGEROMRÅDE                             
005900     03 MOD-IDDISTR-RAD      OCCURS 14 TIMES                              
006000                             PIC Z(3)9.                                   
006100*                                 DISTRIKTNUMMER                          
006200     03 MOD-IDKUNDNR-RAD     OCCURS 14 TIMES                              
006300                             PIC Z(5)9.                                   
006400*                                 KUNDNUMMER                              
006500     03 MOD-IDORDNR7-RAD     OCCURS 14 TIMES                              
006600                             PIC Z(6)9.                                   
006700*                                 ORDERNUMMER                             
006800     03 MOD-IDDC-RAD         OCCURS 14 TIMES                              
006900                             PIC X(2).                                    
007000*                                 IDENTIFIERARE LAGER                     
007100     03 MOD-KDFRAKT-RAD      OCCURS 14 TIMES                              
007200                             PIC Z9.                                      
007300*                                 FRAKTSÄTT DC TILL KUND                  
007400     03 MOD-KDORDKL-RAD      OCCURS 14 TIMES                              
007500                             PIC 9.                                       
007600*                                 ORDERKLASS                              
007700     03 MOD-KDORDSTA-RAD     OCCURS 14 TIMES                              
007800                             PIC X(2).                                    
007900*                                 VOLVOORDERSTATUS                        
008000     03 MOD-KVBEART-Q-RAD    OCCURS 14 TIMES                              
008100                             PIC Z(5)9.                                   
008200*                                 BESTÄLLT KVANTANPASSAT ANTAL            
008300     03 MOD-TIREGDAT-RAD     OCCURS 14 TIMES                              
008400                             PIC 9(6).                                    
008500*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
008600     03 MOD-IDLEVNR-RAD      OCCURS 14 TIMES                              
008700                             PIC X(5).                                    
008800*                                 LEVERANTÖRNUMMER                        
008900     03 MOD-IDORDNR7-URS-RAD OCCURS 14 TIMES                              
009000                             PIC Z(6)9.                                   
009100*                                 ORDERNUMMER                             
009200     03 MOD-IDVIN-RAD        OCCURS 14 TIMES                              
009300                             PIC X(17).                                   
009400*                                 VIN ID FORDON                           
009500     03 MOD-ADGANG-ENTER     PIC 9(3).                                    
009600*                                 GÅNG                                    
009700     03 MOD-ADGANG-NEXT      PIC 9(3).                                    
009800*                                 GÅNG                                    
009900     03 MOD-ADPLATS-ENTER    PIC 9(5).                                    
010000*                                 LAGERPLATSNUMMER                        
010100     03 MOD-ADPLATS-NEXT     PIC 9(5).                                    
010200*                                 LAGERPLATSNUMMER                        
010300     03 MOD-IDPRODNR-ENTER   PIC 9(7).                                    
010400*                                 PRODUKTIONSNUMMER                       
010500     03 MOD-IDPRODNR-NEXT    PIC 9(7).                                    
010600*                                 PRODUKTIONSNUMMER                       
010700     03 MOD-IDDB-ENTER       PIC X(6).                                    
010800*                                 DATABAS                                 
010900     03 MOD-IDDB-NEXT        PIC X(6).                                    
011000*                                 DATABAS                                 
011100     03 MOD-IDKOLLI-ENTER    PIC 9(5).                                    
011200*                                 KOLLINUMMER                             
011300     03 MOD-IDKOLLI-NEXT     PIC 9(5).                                    
011400*                                 KOLLINUMMER                             
011500     03 MOD-IDDISTR-ENTER    PIC 9(4).                                    
011600*                                 DISTRIKTNUMMER                          
011700     03 MOD-IDDISTR-NEXT     PIC 9(4).                                    
011800*                                 DISTRIKTNUMMER                          
011900     03 MOD-TEMFSINF         PIC X(55).                                   
012000*                                 INFORMATIONSMEDDELANDE                  
012100*** END OF VILMAII-COPY LENGTH= 1209 BYTES                                
