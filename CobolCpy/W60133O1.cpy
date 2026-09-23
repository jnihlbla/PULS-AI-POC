000100 01  RESP-W60133O1.                                                       
000200*                                 COPYTEXT FÖR RESP                       
000300*                                 W60133O1                                
000400     03 RESP-IDLEVNR-KOLLI   PIC X(5).                                    
000500*                                 LEVERANTÖRNUMMER KOLLI                  
000600     03 RESP-IDOKOLLI        PIC X(9).                                    
000700*                                 ODETTE KOLLINUMMER                      
000800     03 RESP-IDLOPNRM        PIC X(9).                                    
000900*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001000*                                 (0VVDLLLLK)                             
001100     03 RESP-IDDC            PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300     03 RESP-IDLOPNRM-START  PIC 9(9).                                    
001400*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001500*                                 (0VVDLLLLK)                             
001600     03 RESP-IDLOPNRM-NEXT   PIC 9(9).                                    
001700*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001800*                                 (0VVDLLLLK)                             
001900     03 RESP-IDARTNR         PIC Z(7)9.                                   
002000*                                 ARTIKELNUMMER                           
002100     03 RESP-KVAVIS          PIC Z(6)9.                                   
002200*                                 AVISERAT ANTAL                          
002300     03 RESP-BEART           PIC X(25).                                   
002400*                                 ARTIKELBENÄMNING                        
002500     03 RESP-KDSORT          PIC X(2).                                    
002600*                                 SORT-KOD                                
002700     03 RESP-BEFT            PIC Z9.                                      
002800*                                 FÖRPACKNINGSTYP                         
002900     03 RESP-KVKOLLI-TOT     PIC Z(3)9.                                   
003000*                                 ANTAL KOLLI                             
003100     03 RESP-BEFARLIG        PIC X(10).                                   
003200     03 RESP-KVAVIS-KVAR     PIC Z(6)9.                                   
003300*                                 AVISERAT ANTAL                          
003400     03 RESP-KVAVIS-PRIO-KVAR                                             
003500                             PIC Z(6)9.                                   
003600*                                 AVISERAT ANTAL                          
003700     03 RESP-KDLAGEMB        PIC X(4).                                    
003800*                                 EMBALLAGEBETECKNING                     
003900     03 RESP-ADLAGOMR        PIC Z(2)9.                                   
004000*                                 LAGEROMRÅDE                             
004100     03 RESP-ADGANG          PIC Z(2)9.                                   
004200*                                 GÅNG                                    
004300     03 RESP-ADPLATS         PIC Z(4)9.                                   
004400*                                 LAGERPLATSNUMMER                        
004500     03 RESP-FLKVAANT-TOT    PIC X.                                       
004600*                                 ANTALSKONTROLL UTFÖRD                   
004700     03 RESP-KVKVAPRIM-KVAR  PIC Z(5)9.                                   
004800*                                 ANTAL TILL PRIMÄRKONTROLL               
004900     03 RESP-KVROS           PIC Z(5)9.                                   
005000*                                 RESTORDERSALDO                          
005100     03 RESP-KVAVIS-KIT-KVAR PIC Z(5)9.                                   
005200*                                 AVISERAT ANTAL FÖR SATS                 
005300     03 RESP-ADTRDEST-KIT    PIC X(3).                                    
005400*                                 TRANSPORTDESTINATION SATSER             
005500     03 RESP-ADINLOMR-NXT1   PIC X(4).                                    
005600*                                 INLEVERANSOMRÅDE NÄSTA                  
005700     03 RESP-ADINLOMR-NXT2   PIC X(4).                                    
005800*                                 INLEVERANSOMRÅDE NÄSTA                  
005900     03 RESP-ADINLOMR-NXT3   PIC X(4).                                    
006000*                                 INLEVERANSOMRÅDE NÄSTA                  
006100     03 RESP-ADINLOMR-NXT4   PIC X(4).                                    
006200*                                 INLEVERANSOMRÅDE NÄSTA                  
006300     03 RESP-ADINLOMR-NXT5   PIC X(4).                                    
006400*                                 INLEVERANSOMRÅDE NÄSTA                  
006500     03 RESP-KDKLIPRI        PIC X.                                       
006600*                                 PRIORITETSKOD KOLLI                     
006700     03 RESP-FLSATS          PIC X.                                       
006800*                                 SATSARTIKEL                             
006900     03 RESP-FLKVAANT        PIC X.                                       
007000*                                 ANTALSKONTROLL UTFÖRD                   
007100     03 RESP-KDCMDVAL-INM-ATTR                                            
007200                             PIC X(2).                                    
007300*                                 MFS ATTRIBUTFÄLT                        
007400     03 RESP-KDCMDVAL-INM    PIC X(3).                                    
007500*                                 GENERELL KOMMANDOKOD                    
007600     03 RESP-KVINLART-INM-ATTR                                            
007700                             PIC X(2).                                    
007800*                                 MFS ATTRIBUTFÄLT                        
007900     03 RESP-KVINLART-INM    PIC X(6).                                    
008000*                                 ANTAL I PARTIRAD                        
008100     03 RESP-ADINLOMR-INM-ATTR                                            
008200                             PIC X(2).                                    
008300*                                 MFS ATTRIBUTFÄLT                        
008400     03 RESP-ADINLOMR-INM    PIC X(4).                                    
008500*                                 INLEVERANSOMRÅDE                        
008600     03 RESP-IDINLVGN-INM-ATTR                                            
008700                             PIC X(2).                                    
008800*                                 MFS ATTRIBUTFÄLT                        
008900     03 RESP-IDINLVGN-INM    PIC 9(3).                                    
009000*                                 VAGNSIDENTITET                          
009100     03 RESP-ADINLOMR-NXT-INM-ATTR                                        
009200                             PIC X(2).                                    
009300*                                 MFS ATTRIBUTFÄLT                        
009400     03 RESP-ADINLOMR-NXT-INM                                             
009500                             PIC X(4).                                    
009600*                                 INLEVERANSOMRÅDE                        
009700     03 RESP-KDKLIPRI-INM-ATTR                                            
009800                             PIC X(2).                                    
009900*                                 MFS ATTRIBUTFÄLT                        
010000     03 RESP-KDKLIPRI-INM    PIC X.                                       
010100*                                 PRIORITETSKOD KOLLI                     
010200     03 RESP-FLSATS-INM-ATTR PIC X(2).                                    
010300*                                 MFS ATTRIBUTFÄLT                        
010400     03 RESP-FLSATS-INM      PIC X.                                       
010500*                                 SATSARTIKEL                             
010600     03 RESP-ADINLOMR-PRT-ATTR                                            
010700                             PIC X(2).                                    
010800*                                 MFS ATTRIBUTFÄLT                        
010900     03 RESP-ADINLOMR-PRT    PIC X(4).                                    
011000*                                 INLEVERANSOMRÅDE                        
011100     03 RESP-KDPRTVAL-ATTR   PIC X(2).                                    
011200*                                 MFS ATTRIBUTFÄLT                        
011300     03 RESP-KDPRTVAL        PIC X(2).                                    
011400*                                 MFS BEHANDLING AV INPUTFÄLT             
011500     03 RESP-KVRADER         PIC 9(5).                                    
011600*                                 ANTAL RADER                             
011700     03 RESP-BEPRTLST        PIC X(25).                                   
011800*                                 LOGISK LISTA+PRINTER BENÄMNING          
011900     03 RESP-IDUSER-IN-ATTR  PIC X(2).                                    
012000*                                 MFS ATTRIBUTFÄLT                        
012100     03 RESP-IDUSER-IN       PIC X(8).                                    
012200*                                 ANVÄNDARENS SÄKERHETS ID                
012300     03 RESP-LINE            OCCURS 500 TIMES.                            
012400*                                 LINES                                   
012500        05 RESP-KVINLART-LINE                                             
012600                             PIC Z(5)9.                                   
012700*                                 ANTAL I PARTIRAD                        
012800        05 RESP-KVKOLLI-LINE PIC Z(3)9.                                   
012900*                                 ANTAL KOLLI                             
013000        05 RESP-ADINLOMR-LINE                                             
013100                             PIC X(4).                                    
013200*                                 INLEVERANSOMRÅDE                        
013300        05 RESP-KDINLSTA-LINE                                             
013400                             PIC X(3).                                    
013500*                                 SYSTEMSTATUS INLEVERANS                 
013600        05 RESP-IDINLVGN-LINE                                             
013700                             PIC Z(3).                                    
013800*                                 VAGNSIDENTITET                          
013900        05 RESP-ADINLOMR-NXT-LINE                                         
014000                             PIC X(4).                                    
014100*                                 INLEVERANSOMRÅDE NÄSTA                  
014200*** END OF VILMAII-COPY LENGTH= 12261 BYTES                               
