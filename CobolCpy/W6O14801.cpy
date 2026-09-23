000100 01  MOD-W6O14801.                                                        
000200*                                 COPYTEXT FOR MOD W6O14801               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDLOPNRM-IN      PIC X(8).                                    
000800*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
000900*                                 (0VVDLLLLK)                             
001000     03 MOD-IDLOPNRM-UT      PIC X(8).                                    
001100*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001200*                                 (0VVDLLLLK)                             
001300     03 MOD-IDLEVNR-KOLLI-IN PIC X(5).                                    
001400*                                 LEVERANTÖRNUMMER KOLLI                  
001500     03 MOD-IDLEVNR-KOLLI-UT PIC X(5).                                    
001600*                                 LEVERANTÖRNUMMER KOLLI                  
001700     03 MOD-IDOKOLLI-IN      PIC X(9).                                    
001800*                                 ODETTE KOLLINUMMER                      
001900     03 MOD-IDOKOLLI-UT      PIC X(9).                                    
002000*                                 ODETTE KOLLINUMMER                      
002100     03 MOD-IDDC-IN          PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300     03 MOD-IDDC-UT          PIC X(2).                                    
002400*                                 IDENTIFIERARE LAGER                     
002500     03 MOD-IDRADNR-ENTER    PIC 9(4).                                    
002600*                                 RADNUMMER                               
002700     03 MOD-IDRADNR-NEXT     PIC 9(4).                                    
002800*                                 RADNUMMER                               
002900     03 MOD-IDARTNR          PIC Z(7)9.                                   
003000*                                 ARTIKELNUMMER                           
003100     03 MOD-KVAVIS           PIC Z(5)9.                                   
003200*                                 AVISERAT ANTAL                          
003300     03 MOD-BEART            PIC X(25).                                   
003400*                                 ARTIKELBENÄMNING                        
003500     03 MOD-RAD              OCCURS 12 TIMES.                             
003600*                                 LINES                                   
003700        05 MOD-KDCMDVAL-IN-ATTR                                           
003800                             PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000        05 MOD-KDCMDVAL-IN   PIC X(3).                                    
004100*                                 GENERELL KOMMANDOKOD                    
004200        05 MOD-IDRADNR       PIC Z(3)9.                                   
004300*                                 RADNUMMER                               
004400        05 MOD-IDLEVNR-KOLLI PIC X(5).                                    
004500*                                 LEVERANTÖRNUMMER KOLLI                  
004600        05 MOD-IDOKOLLI      PIC Z(9).                                    
004700*                                 ODETTE KOLLINUMMER                      
004800        05 MOD-KVINLART      PIC Z(5)9.                                   
004900*                                 ANTAL I PARTIRAD                        
005000        05 MOD-ADINLOMR      PIC X(4).                                    
005100*                                 INLEVERANSOMRÅDE                        
005200        05 MOD-FLPRIO        PIC X.                                       
005300*                                 PRIORITERAD                             
005400        05 MOD-KDINLSTA      PIC X(3).                                    
005500*                                 SYSTEMSTATUS INLEVERANS                 
005600     03 MOD-TEMFSINF         PIC X(55).                                   
005700*                                 INFORMATIONSMEDDELANDE                  
005800*** END OF VILMAII-COPY LENGTH= 638 BYTES                                 
