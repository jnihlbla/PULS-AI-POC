000100 01  W4O61101-CTX.                                                        
000200*                                 MOD-COPYTEXT                            
000300*                                 FÖR W40611                              
000400     03 IDTRANS              PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 TEMFSFEL             PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 IDDISTR-IN           PIC X(4).                                    
000900*                                 DISTRIKTNUMMER                          
001000     03 IDDISTR-UT           PIC X(4).                                    
001100*                                 DISTRIKTNUMMER                          
001200     03 IDKUNDNR-IN          PIC X(6).                                    
001300*                                 KUNDNUMMER                              
001400     03 IDKUNDNR-UT          PIC X(6).                                    
001500*                                 KUNDNUMMER                              
001600     03 KDFRAKT-IN           PIC X(2).                                    
001700*                                 FRAKTSÄTT DC TILL KUND                  
001800     03 KDFRAKT-UT           PIC X(2).                                    
001900*                                 FRAKTSÄTT DC TILL KUND                  
002000     03 IDDC-IN              PIC X(2).                                    
002100*                                 IDENTIFIERARE LAGER                     
002200     03 IDDC-UT              PIC X(2).                                    
002300*                                 IDENTIFIERARE LAGER                     
002400     03 SPAR-IDDISTR         PIC 9(4).                                    
002500*                                 DISTRIKTNUMMER                          
002600     03 SPAR-IDKUNDNR        PIC 9(6).                                    
002700*                                 KUNDNUMMER                              
002800     03 SPAR-KDFRAKT         PIC 9(2).                                    
002900*                                 FRAKTSÄTT DC TILL KUND                  
003000     03 SPAR-IDPRODNR-SAMP   PIC 9(7).                                    
003100*                                 PRODUKTIONSNUMMER SAMPACKNING           
003200     03 FLSAMTL-ATTR         PIC X(2).                                    
003300*                                 MFS ATTRIBUTFÄLT                        
003400     03 FLSAMTL              PIC X.                                       
003500*                                 J= SAMTL KOLLI VALDA PÅ ORDERN          
003600     03 W4O61101-001-GRP     OCCURS 13 TIMES.                             
003700        05 KDUPPTYP-ATTR     PIC X(2).                                    
003800*                                 MFS ATTRIBUTFÄLT                        
003900        05 KDUPPTYP          PIC X.                                       
004000*                                 UPPDATERINGSTYP                         
004100        05 IDDISTR           PIC Z(4).                                    
004200*                                 DISTRIKTNUMMER                          
004300        05 IDKUNDNR          PIC Z(6).                                    
004400*                                 KUNDNUMMER                              
004500        05 KDFRAKT           PIC Z(2).                                    
004600*                                 FRAKTSÄTT DC TILL KUND                  
004700        05 IDORDNR           PIC Z(5).                                    
004800*                                 ORDERNUMMER UTGÅR PD90                  
004900        05 KDORDKL           PIC Z.                                       
005000*                                 ORDERKLASS                              
005100        05 IDPRODNR          PIC Z(7).                                    
005200*                                 PRODUKTIONSNUMMER                       
005300        05 KVLADA-CONT       PIC Z(5).                                    
005400*                                 ANTAL KOLLI                             
005500        05 KVPKT             PIC Z(5).                                    
005600*                                 ANTAL KOLLI                             
005700        05 KVBNT-STY         PIC Z(5).                                    
005800*                                 ANTAL KOLLI                             
005900        05 KVHACK            PIC Z(5).                                    
006000*                                 ANTAL KOLLI                             
006100        05 KVPALL            PIC Z(5).                                    
006200*                                 ANTAL KOLLI                             
006300        05 VKORDBTO          PIC Z(6).Z.                                  
006400*                                 ORDERVIKT BRUTTO (KG)                   
006500        05 RADTEXT           PIC X(4).                                    
006600     03 TEMFSINF             PIC X(55).                                   
006700*                                 INFORMATIONSMEDDELANDE                  
006800*** END OF VILMAII-COPY LENGTH= 994 BYTES                                 
