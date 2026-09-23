000100 01  MOD-W90438O1.                                                        
000200*                                 MOD-COPYTEXT FÖR PGM W90438             
000300*                                 SPIE2                                   
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-FELTEXT          PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDARTNR-IN-ATTR  PIC X(2).                                    
000900*                                 MFS ATTRIBUTFÄLT                        
001000     03 MOD-IDARTNR-IN       PIC Z(8).                                    
001100*                                 ARTIKELNUMMER                           
001200     03 MOD-IDARTNR-UT       PIC Z(7)9.                                   
001300*                                 ARTIKELNUMMER                           
001400     03 MOD-IDLEVNR-IN-ATTR  PIC X(2).                                    
001500*                                 MFS ATTRIBUTFÄLT                        
001600     03 MOD-IDLEVNR-IN       PIC X(5).                                    
001700*                                 LEVERANTÖRNUMMER                        
001800     03 MOD-IDLEVNR-UT       PIC X(5).                                    
001900*                                 LEVERANTÖRNUMMER                        
002000     03 MOD-VAL-IN-ATTR      PIC X(2).                                    
002100*                                 MFS ATTRIBUTFÄLT                        
002200     03 MOD-VAL-IN           PIC X.                                       
002300     03 MOD-VAL-UT           PIC X(7).                                    
002400     03 MOD-UTDATARAD.                                                    
002500*                                 ARTIKELDATA                             
002600        05 MOD-BELEV-VCC     PIC X(60).                                   
002700*                                 LEVERANTÖRSNAMN VCC                     
002800        05 MOD-ADLEVLND      PIC X(20).                                   
002900*                                 LEVERANTÖRSADRESS LAND                  
003000        05 MOD-VECKORLT      PIC Z9.                                      
003100*                                 ANTAL VECKOR LEDTID                     
003200        05 MOD-VECKORFT      PIC Z9.                                      
003300*                                 ANTAL VECKOR FRYSNINGSTID               
003400        05 MOD-DAGARINL      PIC Z9.                                      
003500*                                 INLEVERANSTID     (ANTAL DAGAR)         
003600        05 MOD-KVPB-PLAN     PIC Z(6).Z.                                  
003700*                                 PLANERAT PERIODBEHOV                    
003800        05 MOD-KVPB-SATS     PIC Z(6).Z.                                  
003900*                                 SATS-PERIODBEHOV                        
004000        05 MOD-NAMN-OCH-TEL--ON-6108.                                     
004100           07 MOD-IDNAMN-ANSK                                             
004200                             PIC X(40).                                   
004300*                                 NAMN                                    
004400           07 MOD-IDTFN-ANSK PIC X(20).                                   
004500*                                 TELEFONNUMMER EXTERNT                   
004600           07 MOD-IDNAMN-BEREDARE                                         
004700                             PIC X(40).                                   
004800*                                 NAMN                                    
004900           07 MOD-IDTFN-BEREDARE                                          
005000                             PIC X(20).                                   
005100*                                 TELEFONNUMMER EXTERNT                   
005200           07 MOD-IDNAMN-INK PIC X(40).                                   
005300*                                 NAMN                                    
005400           07 MOD-IDTFN-INK  PIC X(20).                                   
005500*                                 TELEFONNUMMER EXTERNT                   
005600           07 MOD-IDNAMN-FORP                                             
005700                             PIC X(40).                                   
005800*                                 NAMN                                    
005900           07 MOD-IDTFN-FORP PIC X(20).                                   
006000*                                 TELEFONNUMMER EXTERNT                   
006100           07 MOD-IDNAMN-KVAL                                             
006200                             PIC X(40).                                   
006300*                                 NAMN                                    
006400           07 MOD-IDTFN-KVAL PIC X(20).                                   
006500*                                 TELEFONNUMMER EXTERNT                   
006600        05 MOD-KDAVT         PIC 9.                                       
006700*                                 AVTALSMÄRKNING                          
006800        05 MOD-LAGERPLATS.                                                
006900           07 MOD-ADLAGOMR   PIC Z(2)9.                                   
007000*                                 LAGEROMRÅDE                             
007100           07 MOD-ADGANG     PIC Z(2)9.                                   
007200*                                 GÅNG                                    
007300           07 MOD-ADPLATS    PIC Z(5)9.                                   
007400*                                                     ADPLATS-002         
007500*                                 LAGERPLATS OMR. GÅNG PLATS              
007600        05 MOD-KOL-1-3--ON-2102                                           
007700                             OCCURS 3 TIMES.                              
007800*                                                                         
007900           07 MOD-ARB-SALDO  PIC -(7)9.                                   
008000*                                 LAGERTILLGÅNG                           
008100           07 MOD-DISP       PIC -(7)9.                                   
008200*                                 DISPONIBELT LAGER                       
008300           07 MOD-KVLS       PIC -(7)9.                                   
008400*                                 LAGERSALDO                              
008500           07 MOD-KVAKS-LAGER                                             
008600                             PIC -(7)9.                                   
008700*                                 ANKOMSTSALDO                            
008800        05 MOD-KVOI-PER--ON-2107                                          
008900                             OCCURS 6 TIMES.                              
009000           07 MOD-AARTAL     PIC X(4).                                    
009100           07 MOD-KVOI-TOT   PIC Z(6)9.                                   
009200*                                 ORDERINGÅNG I STYCK PER TIDSENH         
009300        05 MOD-SUM-LEVERERAT--ON-3304.                                    
009400           07 MOD-SULEVANT-PER                                            
009500                             PIC -(3)B-(3)B-(2)9.                         
009600*                                 ANTAL LEV ART SENASTE PERIOD            
009700*                                 (AF5)                                   
009800           07 MOD-SULEVANT-AAR                                            
009900                             PIC -(3)B-(3)B-(2)9.                         
010000*                                 ANTAL LEVERERADE ARTIKLAR               
010100*                                 HITTILLS DETTA ÅR  (AF4)                
010200           07 MOD-SULEVANT-FAAR                                           
010300                             PIC -(3)B-(3)B-(2)9.                         
010400*                                 ANTAL LEV ART "HITTILLS I ÅR"           
010500*                                 MEN FÖREGÅENDE ÅR (AF3)                 
010600           07 MOD-SULEVANT-RAAR                                           
010700                             PIC -(3)B-(3)B-(2)9.                         
010800*                                 ANTAL LEV ART RULLANDE ÅR               
010900*                                 (AF2)                                   
011000           07 MOD-SULEVANT-FRAAR                                          
011100                             PIC -(3)B-(3)B-(2)9.                         
011200*                                 ANTAL LEVERERADE ARTIKLAR               
011300*                                 FÖREGÅENDE RULLANDE ÅR (AF1)            
011400     03 MOD-MEDDELANDE       PIC X(55).                                   
011500*                                 INFORMATIONSMEDDELANDE                  
011600*** END OF VILMAII-COPY LENGTH= 771 BYTES                                 
