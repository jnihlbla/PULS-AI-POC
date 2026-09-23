000100 01  MOD-W6O13701.                                                        
000200*                                 MODCOPYTEXT TILL W60137.                
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDLEVNR-KOLLI-IN PIC X(5).                                    
000800*                                 LEVERANTÖRNUMMER                        
000900     03 MOD-IDOKOLLI-IN      PIC X(9).                                    
001000*                                 ODETTE KOLLINUMMER                      
001100     03 MOD-IDDC-IN          PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300     03 MOD-IDLEVNR-KOLLI-UT PIC X(5).                                    
001400*                                 LEVERANTÖRNUMMER                        
001500     03 MOD-IDOKOLLI-UT      PIC X(9).                                    
001600*                                 ODETTE KOLLINUMMER                      
001700     03 MOD-IDDC-UT          PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900     03 MOD-ADLAGOMR-UPD-ATTR                                             
002000                             PIC X(2).                                    
002100*                                 MFS ATTRIBUTFÄLT                        
002200     03 MOD-ADLAGOMR-UPD     PIC X(2).                                    
002300*                                 LAGEROMRÅDE                             
002400     03 MOD-ADINLOMR-UPD-ATTR                                             
002500                             PIC X(2).                                    
002600*                                 MFS ATTRIBUTFÄLT                        
002700     03 MOD-ADINLOMR-UPD     PIC X(4).                                    
002800*                                 INLEVERANSOMRÅDE                        
002900     03 MOD-FLPRIO-UPD-ATTR  PIC X(2).                                    
003000*                                 MFS ATTRIBUTFÄLT                        
003100     03 MOD-FLPRIO-UPD       PIC X.                                       
003200*                                 PRIORITERAD                             
003300     03 MOD-ADLAGOMR-ATTR    PIC X(2).                                    
003400*                                 MFS ATTRIBUTFÄLT                        
003500     03 MOD-ADLAGOMR         PIC X(2).                                    
003600*                                 LAGEROMRÅDE                             
003700     03 MOD-UPDATE           OCCURS 11 TIMES.                             
003800*                                 UPDATE                                  
003900        05 MOD-IDLOPNRM-RAD-ATTR                                          
004000                             PIC X(2).                                    
004100*                                 MFS ATTRIBUTFÄLT                        
004200        05 MOD-RAD.                                                       
004300*                                 UPDATE                                  
004400           07 MOD-IDLOPNRM-RAD                                            
004500                             PIC X(8).                                    
004600*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
004700*                                 (0VVDLLLLK)                             
004800           07 MOD-IDRADNR-RAD                                             
004900                             PIC X(3).                                    
005000*                                 RADNUMMER                               
005100     03 MOD-ADINLOMR-PRT-ATTR                                             
005200                             PIC X(2).                                    
005300*                                 MFS ATTRIBUTFÄLT                        
005400     03 MOD-ADINLOMR-PRT     PIC X(4).                                    
005500*                                 PRINTERPLACERING                        
005600     03 MOD-IDINLVGN-IN      PIC X(3).                                    
005700*                                 VAGNSIDENTITET                          
005800     03 MOD-IDINLVGN-UT      PIC X(3).                                    
005900*                                 VAGNSIDENTITET                          
006000     03 MOD-ADINLOMR-IN      PIC X(4).                                    
006100*                                 INLEVERANSOMRÅDE                        
006200     03 MOD-ADINLOMR-UT      PIC X(4).                                    
006300*                                 INLEVERANSOMRÅDE                        
006400     03 MOD-ADINLOMR-NXT-IN  PIC X(4).                                    
006500*                                 INLEVERANSOMRÅDE NÄSTA                  
006600     03 MOD-ADINLOMR-NXT-UT  PIC X(4).                                    
006700*                                 INLEVERANSOMRÅDE NÄSTA                  
006800     03 MOD-KDINLQ-IN        PIC X.                                       
006900     03 MOD-KDINLQ-UT        PIC X.                                       
007000     03 MOD-BEFT-FOM-IN      PIC X(2).                                    
007100*                                 FÖRPACKNINGSTYP                         
007200     03 MOD-BEFT-FOM-UT      PIC X(2).                                    
007300*                                 FÖRPACKNINGSTYP                         
007400     03 MOD-BEFT-TOM-IN      PIC X(2).                                    
007500*                                 FÖRPACKNINGSTYP                         
007600     03 MOD-BEFT-TOM-UT      PIC X(2).                                    
007700*                                 FÖRPACKNINGSTYP                         
007800     03 MOD-FLINLFB-IN       PIC X.                                       
007900*                                 VALD TILL FÖRBEHANDLING                 
008000     03 MOD-FLINLFB-UT       PIC X.                                       
008100*                                 VALD TILL FÖRBEHANDLING                 
008200     03 MOD-IDLOPNRM-IN      PIC X(9).                                    
008300*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
008400*                                 (0VVDLLLLK)                             
008500     03 MOD-IDLOPNRM-UT      PIC X(9).                                    
008600*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
008700*                                 (0VVDLLLLK)                             
008800     03 MOD-TEMFSINF         PIC X(55).                                   
008900*                                 INFORMATIONSMEDDELANDE                  
