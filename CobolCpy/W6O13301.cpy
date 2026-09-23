000100 01  MOD-W6O13301.                                                        
000200*                                 COPYTEXT FÖR MOD                        
000300*                                 W6O13301                                
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDLEVNR-KOLLI-IN PIC X(5).                                    
000900*                                 LEVERANTÖRNUMMER KOLLI                  
001000     03 MOD-IDLEVNR-KOLLI-UT PIC X(5).                                    
001100*                                 LEVERANTÖRNUMMER KOLLI                  
001200     03 MOD-IDOKOLLI-IN      PIC X(9).                                    
001300*                                 ODETTE KOLLINUMMER                      
001400     03 MOD-IDOKOLLI-UT      PIC X(9).                                    
001500*                                 ODETTE KOLLINUMMER                      
001600     03 MOD-IDLOPNRM-IN      PIC X(9).                                    
001700*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001800*                                 (0VVDLLLLK)                             
001900     03 MOD-IDLOPNRM-UT      PIC X(9).                                    
002000*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
002100*                                 (0VVDLLLLK)                             
002200     03 MOD-IDDC-IN          PIC X(2).                                    
002300*                                 IDENTIFIERARE LAGER                     
002400     03 MOD-IDDC-UT          PIC X(2).                                    
002500*                                 IDENTIFIERARE LAGER                     
002600     03 MOD-IDLOPNRM-ENTER   PIC 9(9).                                    
002700*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
002800*                                 (0VVDLLLLK)                             
002900     03 MOD-IDLOPNRM-NEXT    PIC 9(9).                                    
003000*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
003100*                                 (0VVDLLLLK)                             
003200     03 MOD-IDARTNR          PIC Z(7)9.                                   
003300*                                 ARTIKELNUMMER                           
003400     03 MOD-KVAVIS           PIC Z(6)9.                                   
003500*                                 AVISERAT ANTAL                          
003600     03 MOD-BEART            PIC X(25).                                   
003700*                                 ARTIKELBENÄMNING                        
003800     03 MOD-KDSORT           PIC X(2).                                    
003900*                                 SORT-KOD                                
004000     03 MOD-BEFT             PIC Z9.                                      
004100*                                 FÖRPACKNINGSTYP                         
004200     03 MOD-KVKOLLI-TOT      PIC Z(3)9.                                   
004300*                                 ANTAL KOLLI                             
004400     03 MOD-BEFARLIG         PIC X(10).                                   
004500     03 MOD-KVAVIS-KVAR      PIC Z(6)9.                                   
004600*                                 AVISERAT ANTAL                          
004700     03 MOD-KVAVIS-PRIO-KVAR PIC Z(6)9.                                   
004800*                                 AVISERAT ANTAL                          
004900     03 MOD-KDLAGEMB         PIC X(4).                                    
005000*                                 EMBALLAGEBETECKNING                     
005100     03 MOD-ADLAGOMR         PIC Z(2)9.                                   
005200*                                 LAGEROMRÅDE                             
005300     03 MOD-ADGANG           PIC Z(2)9.                                   
005400*                                 GÅNG                                    
005500     03 MOD-ADPLATS          PIC Z(4)9.                                   
005600*                                 LAGERPLATSNUMMER                        
005700     03 MOD-FLKVAANT-TOT     PIC X.                                       
005800*                                 ANTALSKONTROLL UTFÖRD                   
005900     03 MOD-KVKVAPRIM-KVAR   PIC Z(5)9.                                   
006000*                                 ANTAL TILL PRIMÄRKONTROLL               
006100     03 MOD-KVROS            PIC Z(5)9.                                   
006200*                                 RESTORDERSALDO                          
006300     03 MOD-KVAVIS-KIT-KVAR  PIC Z(5)9.                                   
006400*                                 AVISERAT ANTAL FÖR SATS                 
006500     03 MOD-ADTRDEST-KIT     PIC X(3).                                    
006600*                                 TRANSPORTDESTINATION SATSER             
006700     03 MOD-ADINLOMR-NXT1    PIC X(4).                                    
006800*                                 INLEVERANSOMRÅDE NÄSTA                  
006900     03 MOD-ADINLOMR-NXT2    PIC X(4).                                    
007000*                                 INLEVERANSOMRÅDE NÄSTA                  
007100     03 MOD-ADINLOMR-NXT3    PIC X(4).                                    
007200*                                 INLEVERANSOMRÅDE NÄSTA                  
007300     03 MOD-ADINLOMR-NXT4    PIC X(4).                                    
007400*                                 INLEVERANSOMRÅDE NÄSTA                  
007500     03 MOD-ADINLOMR-NXT5    PIC X(4).                                    
007600*                                 INLEVERANSOMRÅDE NÄSTA                  
007700     03 MOD-KVINLART         OCCURS 7 TIMES                               
007800                             PIC Z(5)9.                                   
007900*                                 ANTAL I PARTIRAD                        
008000     03 MOD-KVKOLLI          OCCURS 7 TIMES                               
008100                             PIC Z(3)9.                                   
008200*                                 ANTAL KOLLI                             
008300     03 MOD-ADINLOMR         OCCURS 7 TIMES                               
008400                             PIC X(4).                                    
008500*                                 INLEVERANSOMRÅDE                        
008600     03 MOD-KDINLSTA         OCCURS 7 TIMES                               
008700                             PIC X(3).                                    
008800*                                 SYSTEMSTATUS INLEVERANS                 
008900     03 MOD-IDINLVGN         OCCURS 7 TIMES                               
009000                             PIC Z(3).                                    
009100*                                 VAGNSIDENTITET                          
009200     03 MOD-ADINLOMR-NXT     OCCURS 7 TIMES                               
009300                             PIC X(4).                                    
009400*                                 INLEVERANSOMRÅDE NÄSTA                  
009500     03 MOD-KDKLIPRI         PIC X.                                       
009600*                                 PRIORITETSKOD KOLLI                     
009700     03 MOD-FLSATS           PIC X.                                       
009800*                                 SATSARTIKEL                             
009900     03 MOD-FLKVAANT         PIC X.                                       
010000*                                 ANTALSKONTROLL UTFÖRD                   
010100     03 MOD-KDCMDVAL-INM-ATTR                                             
010200                             PIC X(2).                                    
010300*                                 MFS ATTRIBUTFÄLT                        
010400     03 MOD-KDCMDVAL-INM     PIC X(3).                                    
010500*                                 GENERELL KOMMANDOKOD                    
010600     03 MOD-KVINLART-INM-ATTR                                             
010700                             PIC X(2).                                    
010800*                                 MFS ATTRIBUTFÄLT                        
010900     03 MOD-KVINLART-INM     PIC X(6).                                    
011000*                                 ANTAL I PARTIRAD                        
011100     03 MOD-ADINLOMR-INM-ATTR                                             
011200                             PIC X(2).                                    
011300*                                 MFS ATTRIBUTFÄLT                        
011400     03 MOD-ADINLOMR-INM     PIC X(4).                                    
011500*                                 INLEVERANSOMRÅDE                        
011600     03 MOD-IDINLVGN-INM-ATTR                                             
011700                             PIC X(2).                                    
011800*                                 MFS ATTRIBUTFÄLT                        
011900     03 MOD-IDINLVGN-INM     PIC 9(3).                                    
012000*                                 VAGNSIDENTITET                          
012100     03 MOD-ADINLOMR-NXT-INM-ATTR                                         
012200                             PIC X(2).                                    
012300*                                 MFS ATTRIBUTFÄLT                        
012400     03 MOD-ADINLOMR-NXT-INM PIC X(4).                                    
012500*                                 INLEVERANSOMRÅDE                        
012600     03 MOD-KDKLIPRI-INM-ATTR                                             
012700                             PIC X(2).                                    
012800*                                 MFS ATTRIBUTFÄLT                        
012900     03 MOD-KDKLIPRI-INM     PIC X.                                       
013000*                                 PRIORITETSKOD KOLLI                     
013100     03 MOD-FLSATS-INM-ATTR  PIC X(2).                                    
013200*                                 MFS ATTRIBUTFÄLT                        
013300     03 MOD-FLSATS-INM       PIC X.                                       
013400*                                 SATSARTIKEL                             
013500     03 MOD-ADINLOMR-PRT-ATTR                                             
013600                             PIC X(2).                                    
013700*                                 MFS ATTRIBUTFÄLT                        
013800     03 MOD-ADINLOMR-PRT     PIC X(4).                                    
013900*                                 INLEVERANSOMRÅDE                        
014000     03 MOD-KDPRTVAL-ATTR    PIC X(2).                                    
014100*                                 MFS ATTRIBUTFÄLT                        
014200     03 MOD-KDPRTVAL         PIC X(2).                                    
014300*                                 MFS BEHANDLING AV INPUTFÄLT             
014400     03 MOD-IDINLVGN-IN      PIC 9(3).                                    
014500*                                 VAGNSIDENTITET                          
014600     03 MOD-IDINLVGN-UT      PIC 9(3).                                    
014700*                                 VAGNSIDENTITET                          
014800     03 MOD-ADINLOMR-IN      PIC X(4).                                    
014900*                                 INLEVERANSOMRÅDE                        
015000     03 MOD-ADINLOMR-UT      PIC X(4).                                    
015100*                                 INLEVERANSOMRÅDE                        
015200     03 MOD-ADINLOMR-NXT-IN  PIC X(4).                                    
015300*                                 INLEVERANSOMRÅDE                        
015400     03 MOD-ADINLOMR-NXT-UT  PIC X(4).                                    
015500*                                 INLEVERANSOMRÅDE                        
015600     03 MOD-KDINLQ-IN        PIC X.                                       
015700     03 MOD-KDINLQ-UT        PIC X.                                       
015800     03 MOD-BEFT-FOM-IN      PIC X(2).                                    
015900*                                 FÖRPACKNINGSTYP                         
016000     03 MOD-BEFT-FOM-UT      PIC X(2).                                    
016100*                                 FÖRPACKNINGSTYP                         
016200     03 MOD-BEFT-TOM-IN      PIC X(2).                                    
016300*                                 FÖRPACKNINGSTYP                         
016400     03 MOD-BEFT-TOM-UT      PIC X(2).                                    
016500*                                 FÖRPACKNINGSTYP                         
016600     03 MOD-FLINLFB-IN       PIC X.                                       
016700*                                 VALD TILL FÖRBEHANDLING                 
016800     03 MOD-FLINLFB-UT       PIC X.                                       
016900*                                 VALD TILL FÖRBEHANDLING                 
017000     03 MOD-TEMFSINF         PIC X(55).                                   
017100*                                 INFORMATIONSMEDDELANDE                  
017200*** END OF VILMAII-COPY LENGTH= 547 BYTES                                 
