000100 01  MOD-W6O13501.                                                        
000200*                                 MOD-COPYTEXT FÖR W6013500               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDC-IN          PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 MOD-IDDC-UT          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MOD-IDLOPNRM-IN      PIC X(9).                                    
001200*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001300*                                 (0VVDLLLLK)                             
001400     03 MOD-IDLOPNRM-UT      PIC X(9).                                    
001500*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001600*                                 (0VVDLLLLK)                             
001700     03 MOD-RAD              OCCURS 14 TIMES.                             
001800        05 MOD-KOL-GRP       OCCURS 2 TIMES.                              
001900           07 MOD-IDLEVNR-KOLLI-ATTR                                      
002000                             PIC X(2).                                    
002100*                                 MFS ATTRIBUTFÄLT                        
002200           07 MOD-IDLEVNR-KOLLI                                           
002300                             PIC X(5).                                    
002400*                                 LEVERANTÖRNUMMER KOLLI                  
002500           07 MOD-IDOKOLLI-ATTR                                           
002600                             PIC X(2).                                    
002700*                                 MFS ATTRIBUTFÄLT                        
002800           07 MOD-IDOKOLLI   PIC X(9).                                    
002900*                                 ODETTE KOLLINUMMER                      
003000     03 MOD-FLGODK-ATTR      PIC X(2).                                    
003100*                                 MFS ATTRIBUTFÄLT                        
003200     03 MOD-FLGODK           PIC X.                                       
003300     03 MOD-ADINLOMR-PRT     PIC X(4).                                    
003400*                                 PRINTERPLACERING                        
003500     03 MOD-IDINLVGN-IN      PIC X(3).                                    
003600*                                 VAGNSIDENTITET                          
003700     03 MOD-IDINLVGN-UT      PIC X(3).                                    
003800*                                 VAGNSIDENTITET                          
003900     03 MOD-ADINLOMR-IN      PIC X(4).                                    
004000*                                 INLEVERANSOMRÅDE                        
004100     03 MOD-ADINLOMR-UT      PIC X(4).                                    
004200*                                 INLEVERANSOMRÅDE                        
004300     03 MOD-ADINLOMR-NXT-IN  PIC X(4).                                    
004400*                                 INLEVERANSOMRÅDE NÄSTA                  
004500     03 MOD-ADINLOMR-NXT-UT  PIC X(4).                                    
004600*                                 INLEVERANSOMRÅDE NÄSTA                  
004700     03 MOD-KDINLQ-IN        PIC X.                                       
004800     03 MOD-KDINLQ-UT        PIC X.                                       
004900     03 MOD-BEFT-FOM-IN      PIC X(2).                                    
005000*                                 FÖRPACKNINGSTYP                         
005100     03 MOD-BEFT-FOM-UT      PIC X(2).                                    
005200*                                 FÖRPACKNINGSTYP                         
005300     03 MOD-BEFT-TOM-IN      PIC X(2).                                    
005400*                                 FÖRPACKNINGSTYP                         
005500     03 MOD-BEFT-TOM-UT      PIC X(2).                                    
005600*                                 FÖRPACKNINGSTYP                         
005700     03 MOD-FLINLFB-IN       PIC X.                                       
005800*                                 VALD TILL FÖRBEHANDLING                 
005900     03 MOD-FLINLFB-UT       PIC X.                                       
006000*                                 VALD TILL FÖRBEHANDLING                 
006100     03 MOD-IDLEVNR-KOLLI-IN PIC X(5).                                    
006200*                                 LEVERANTÖRNUMMER                        
006300     03 MOD-IDLEVNR-KOLLI-UT PIC X(5).                                    
006400*                                 LEVERANTÖRNUMMER                        
006500     03 MOD-IDOKOLLI-IN      PIC X(9).                                    
006600*                                 ODETTE KOLLINUMMER                      
006700     03 MOD-IDOKOLLI-UT      PIC X(9).                                    
006800*                                 ODETTE KOLLINUMMER                      
006900     03 MOD-TEMFSINF         PIC X(55).                                   
007000*                                 INFORMATIONSMEDDELANDE                  
