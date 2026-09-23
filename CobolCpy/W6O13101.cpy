000100 01  MOD-W6O13101.                                                        
000200*                                 COPYTEXT FÖR MOD                        
000300*                                 W6O13101                                
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDINLVGN-IN      PIC X(2).                                    
000900*                                 MFS BEHANDLING AV INPUTFÄLT             
001000     03 MOD-IDINLVGN-UT      PIC 9(3).                                    
001100*                                 VAGNSIDENTITET                          
001200     03 MOD-IDDC-IN          PIC X(2).                                    
001300*                                 MFS BEHANDLING AV INPUTFÄLT             
001400     03 MOD-IDDC-UT          PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600     03 MOD-IDLOPNRM-NEXT    PIC X(9).                                    
001700*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001800*                                 (0VVDLLLLK)                             
001900     03 MOD-IDRADNR-NEXT     PIC 9(4).                                    
002000*                                 RADNUMMER                               
002100     03 MOD-IDLOPNRM-ENTER   PIC X(9).                                    
002200*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
002300*                                 (0VVDLLLLK)                             
002400     03 MOD-IDRADNR-ENTER    PIC 9(4).                                    
002500*                                 RADNUMMER                               
002600     03 MOD-RAD              OCCURS 12 TIMES.                             
002700        05 MOD-IDARTNR-ATTR  PIC X(2).                                    
002800*                                 MFS ATTRIBUTFÄLT                        
002900        05 MOD-IDARTNR       PIC Z(7)9.                                   
003000*                                 ARTIKELNUMMER                           
003100        05 MOD-KVINLART      PIC Z(5)9.                                   
003200*                                 ANTAL I PARTIRAD                        
003300        05 MOD-KVAVIS-PRIO   PIC Z(5)9.                                   
003400*                                 BERÄKN PRIORITERAD KVANT TOT            
003500        05 MOD-BEFT          PIC Z9.                                      
003600*                                 FÖRPACKNINGSTYP                         
003700        05 MOD-FLKVAANT-TOT  PIC X.                                       
003800*                                 ANTALSKONTROLL UTFÖRD                   
003900        05 MOD-KVKVAPRIM     PIC Z(5)9.                                   
004000*                                 ANTAL TILL PRIMÄRKONTROLL               
004100        05 MOD-KVROS         PIC Z(5)9.                                   
004200*                                 RESTORDERSALDO                          
004300        05 MOD-KVAVIS-KIT    PIC Z(5)9.                                   
004400*                                 AVISERAT ANTAL FÖR SATS                 
004500        05 MOD-ADTRDEST-KIT  PIC X(3).                                    
004600*                                 TRANSPORTDESTINATION SATSER             
004700        05 MOD-KDLAGEMB      PIC X(4).                                    
004800*                                 EMBALLAGEBETECKNING                     
004900        05 MOD-ADINLOMR-NXT1 PIC X(4).                                    
005000*                                 INLEVERANSOMRÅDE                        
005100        05 MOD-ADINLOMR-NXT2 PIC X(4).                                    
005200*                                 INLEVERANSOMRÅDE                        
005300        05 MOD-ADINLOMR-NXT3 PIC X(4).                                    
005400*                                 INLEVERANSOMRÅDE                        
005500     03 MOD-ADINLOMR-TOMN-ATTR                                            
005600                             PIC X(2).                                    
005700*                                 MFS ATTRIBUTFÄLT                        
005800     03 MOD-ADINLOMR-TOMN    PIC X(4).                                    
005900*                                 INLEVERANSOMRÅDE                        
006000     03 MOD-ADINLOMR-PRT     PIC X(4).                                    
006100*                                 PRINTERPLACERING                        
006200     03 MOD-ADINLOMR-IN      PIC X(4).                                    
006300*                                 INLEVERANSOMRÅDE                        
006400     03 MOD-ADINLOMR-UT      PIC X(4).                                    
006500*                                 INLEVERANSOMRÅDE                        
006600     03 MOD-ADINLOMR-NXT-IN  PIC X(4).                                    
006700*                                 INLEVERANSOMRÅDE NÄSTA                  
006800     03 MOD-ADINLOMR-NXT-UT  PIC X(4).                                    
006900*                                 INLEVERANSOMRÅDE NÄSTA                  
007000     03 MOD-KDINLQ-IN        PIC X.                                       
007100     03 MOD-KDINLQ-UT        PIC X.                                       
007200     03 MOD-BEFT-FOM-IN      PIC X(2).                                    
007300*                                 FÖRPACKNINGSTYP                         
007400     03 MOD-BEFT-FOM-UT      PIC X(2).                                    
007500*                                 FÖRPACKNINGSTYP                         
007600     03 MOD-BEFT-TOM-IN      PIC X(2).                                    
007700*                                 FÖRPACKNINGSTYP                         
007800     03 MOD-BEFT-TOM-UT      PIC X(2).                                    
007900*                                 FÖRPACKNINGSTYP                         
008000     03 MOD-FLINLFB-IN       PIC X.                                       
008100*                                 VALD TILL FÖRBEHANDLING                 
008200     03 MOD-FLINLFB-UT       PIC X.                                       
008300*                                 VALD TILL FÖRBEHANDLING                 
008400     03 MOD-IDLEVNR-KOLLI-IN PIC X(5).                                    
008500*                                 LEVERANTÖRNUMMER                        
008600     03 MOD-IDLEVNR-KOLLI-UT PIC X(5).                                    
008700*                                 LEVERANTÖRNUMMER                        
008800     03 MOD-IDOKOLLI-IN      PIC X(9).                                    
008900*                                 ODETTE KOLLINUMMER                      
009000     03 MOD-IDOKOLLI-UT      PIC X(9).                                    
009100*                                 ODETTE KOLLINUMMER                      
009200     03 MOD-IDLOPNRM-IN      PIC 9(9).                                    
009300*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
009400*                                 (0VVDLLLLK)                             
009500     03 MOD-IDLOPNRM-UT      PIC 9(9).                                    
009600*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
009700*                                 (0VVDLLLLK)                             
009800     03 MOD-TEMFSINF         PIC X(55).                                   
009900*                                 INFORMATIONSMEDDELANDE                  
010000*** END OF VILMAII-COPY LENGTH= 962 BYTES                                 
