000100 01  MOD-W6O12301-CTX.                                                    
000200*                                 COPYTEXT FOR MOD W6O12301               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-W6O12001-001-GRP.                                             
000800*                                 LINES                                   
000900        05 MOD-IDARTNR-IN    PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100        05 MOD-IDARTNR-UT    PIC X(9).                                    
001200*                                 ARTIKELNUMMER                           
001300        05 MOD-IDLOPNRM-IN   PIC X(8).                                    
001400*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001500*                                 (0VVDLLLLK)                             
001600        05 MOD-IDLOPNRM-UT   PIC X(8).                                    
001700*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001800*                                 (0VVDLLLLK)                             
001900        05 MOD-IDLEVNR-IN    PIC X(5).                                    
002000*                                 LEVERANTÖRNUMMER                        
002100        05 MOD-IDLEVNR-UT    PIC X(5).                                    
002200*                                 LEVERANTÖRNUMMER                        
002300        05 MOD-IDFS-IN       PIC X(8).                                    
002400*                                 FÖLJESEDELSNUMMER ENL ODETTE            
002500        05 MOD-IDFS-UT       PIC X(8).                                    
002600*                                 FÖLJESEDELSNUMMER ENL ODETTE            
002700        05 MOD-IDLBBET-IN    PIC X(12).                                   
002800*                                 LASTBÄRARBETECKNING                     
002900        05 MOD-IDLBBET-UT    PIC X(12).                                   
003000*                                 LASTBÄRARBETECKNING                     
003100        05 MOD-IDDC-IN       PIC X(2).                                    
003200*                                 IDENTIFIERARE LAGER                     
003300        05 MOD-IDDC-UT       PIC X(2).                                    
003400*                                 IDENTIFIERARE LAGER                     
003500        05 MOD-ADINLOMR-PRT-ATTR                                          
003600                             PIC X(2).                                    
003700*                                 MFS ATTRIBUTFÄLT                        
003800        05 MOD-ADINLOMR-PRT  PIC X(4).                                    
003900*                                 PRINTERPLACERING                        
004000     03 MOD-BEART            PIC X(25).                                   
004100*                                 ARTIKELBENÄMNING                        
004200     03 MOD-KDLAGEMB         PIC X(4).                                    
004300*                                 EMBALLAGEBETECKNING                     
004400     03 MOD-ADLAGOMR         PIC X(2).                                    
004500*                                 LAGEROMRÅDE                             
004600     03 MOD-ADGANG           PIC X(2).                                    
004700*                                 GÅNG                                    
004800     03 MOD-ADPLATS          PIC X(5).                                    
004900*                                 LAGERPLATSNUMMER                        
005000     03 MOD-KDSORT           PIC X(2).                                    
005100*                                 SORT-KOD                                
005200     03 MOD-KDFARLIG-TXT     PIC X(10).                                   
005300     03 MOD-W6O12301-001-GRP OCCURS 11 TIMES.                             
005400*                                 LINES                                   
005500        05 MOD-KDCMDVAL-INPUT-ATTR                                        
005600                             PIC X(2).                                    
005700*                                 MFS ATTRIBUTFÄLT                        
005800        05 MOD-KDCMDVAL-INPUT                                             
005900                             PIC X(3).                                    
006000*                                 GENERELL KOMMANDOKOD                    
006100        05 MOD-IDLOPNRM      PIC X(9).                                    
006200*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
006300*                                 (0VVDLLLLK)                             
006400        05 MOD-IDRADNR       PIC X(3).                                    
006500*                                 RADNUMMER                               
006600        05 MOD-IDLEVNR       PIC X(5).                                    
006700*                                 LEVERANTÖRNUMMER                        
006800        05 MOD-IDOKOLLI      PIC X(9).                                    
006900*                                 ODETTE KOLLINUMMER                      
007000        05 MOD-KVINLART      PIC Z(5)9.                                   
007100*                                 ANTAL I PARTIRAD                        
007200        05 MOD-ADINLOMR      PIC X(4).                                    
007300*                                 INLEVERANSOMRÅDE                        
007400        05 MOD-IDINLVGN      PIC X(3).                                    
007500*                                 VAGNSIDENTITET                          
007600        05 MOD-ADINLOMR-NXT  PIC X(4).                                    
007700*                                 INLEVERANSOMRÅDE NÄSTA                  
007800        05 MOD-KDKLIPRI-ATTR PIC X(2).                                    
007900*                                 MFS ATTRIBUTFÄLT                        
008000        05 MOD-KDKLIPRI      PIC X.                                       
008100*                                 PRIORITETSKOD KOLLI                     
008200        05 MOD-FLSATS-ATTR   PIC X(2).                                    
008300*                                 MFS ATTRIBUTFÄLT                        
008400        05 MOD-FLSATS        PIC X.                                       
008500*                                 SATSARTIKEL                             
008600        05 MOD-KDINLSTA-ATTR PIC X(2).                                    
008700*                                 MFS ATTRIBUTFÄLT                        
008800        05 MOD-KDINLSTA      PIC X(3).                                    
008900*                                 SYSTEMSTATUS INLEVERANS                 
009000     03 MOD-TEMFSINF         PIC X(55).                                   
009100*                                 INFORMATIONSMEDDELANDE                  
009200*** END OF VILMAII-COPY LENGTH= 892 BYTES                                 
