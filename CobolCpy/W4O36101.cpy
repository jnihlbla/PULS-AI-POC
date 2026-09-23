000100 01  W4O36101.                                                            
000200*                                 COPYTEXT FÖR MOD                        
000300*                                 W4O36101                                
000400     03 IDTRANS              PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 TEMFSFEL             PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 IDPKLTAB-IN          PIC X(2).                                    
000900*                                 PRODUKTIONSKLASSTABELLSID               
001000     03 IDPKLTAB-UT          PIC X(2).                                    
001100*                                 PRODUKTIONSKLASSTABELLSID               
001200     03 IDDC-IN              PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400     03 IDDC-UT              PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600     03 IDRADNR-DOLD         PIC 9(5).                                    
001700*                                 RADNUMMER                               
001800     03 IDRADNR-DOLD-X       PIC 9(5).                                    
001900*                                 RADNUMMER                               
002000     03 AREA                 OCCURS 13 TIMES.                             
002100        05 IDRADNR-ATTR      PIC X(2).                                    
002200*                                 MFS ATTRIBUTFÄLT                        
002300        05 IDRADNR           PIC 9(5).                                    
002400*                                 RADNUMMER                               
002500        05 KDORDKL-ATTR      PIC X(2).                                    
002600*                                 MFS ATTRIBUTFÄLT                        
002700        05 KDORDKL           PIC 9.                                       
002800*                                 ORDERKLASS                              
002900        05 KVRADER-ATTR      PIC X(2).                                    
003000*                                 MFS ATTRIBUTFÄLT                        
003100        05 KVRADER           PIC Z(4)9.                                   
003200*                                 ANTAL RADER                             
003300        05 VKORDNTO-ATTR     PIC X(2).                                    
003400*                                 MFS ATTRIBUTFÄLT                        
003500        05 VKORDNTO          PIC Z(5)9.9.                                 
003600*                                 ORDERVIKT NETTO (KG)                    
003700        05 VLORDNTO-ATTR     PIC X(2).                                    
003800*                                 MFS ATTRIBUTFÄLT                        
003900        05 VLORDNTO          PIC Z(3)9.9(3).                              
004000*                                 ORDERVOLYM NETTO (M3)                   
004100        05 KDPRODKL-ATTR     PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300        05 KDPRODKL          PIC X.                                       
004400*                                 PRODUKTIONSKLASS                        
004500        05 IDHLOTAB-ATTR     PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700        05 IDHLOTAB          PIC Z9.                                      
004800*                                 HLOTABELLSIDENTITET                     
004900     03 IDRADNR-IN-ATTR      PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100     03 IDRADNR-IN           PIC X(2).                                    
005200*                                 MFS BEHANDLING AV INPUTFÄLT             
005300     03 KDORDKL-IN-ATTR      PIC X(2).                                    
005400*                                 MFS ATTRIBUTFÄLT                        
005500     03 KDORDKL-IN           PIC X(2).                                    
005600*                                 MFS BEHANDLING AV INPUTFÄLT             
005700     03 KVRADER-IN-ATTR      PIC X(2).                                    
005800*                                 MFS ATTRIBUTFÄLT                        
005900     03 KVRADER-IN           PIC X(2).                                    
006000*                                 MFS BEHANDLING AV INPUTFÄLT             
006100     03 VKORDNTO-IN-ATTR     PIC X(2).                                    
006200*                                 MFS ATTRIBUTFÄLT                        
006300     03 VKORDNTO-IN          PIC X(2).                                    
006400*                                 MFS BEHANDLING AV INPUTFÄLT             
006500     03 VLORDNTO-IN-ATTR     PIC X(2).                                    
006600*                                 MFS ATTRIBUTFÄLT                        
006700     03 VLORDNTO-IN          PIC X(2).                                    
006800*                                 MFS BEHANDLING AV INPUTFÄLT             
006900     03 KDPRODKL-IN-ATTR     PIC X(2).                                    
007000*                                 MFS ATTRIBUTFÄLT                        
007100     03 KDPRODKL-IN          PIC X(2).                                    
007200*                                 MFS BEHANDLING AV INPUTFÄLT             
007300     03 IDHLOTAB-IN-ATTR     PIC X(2).                                    
007400*                                 MFS ATTRIBUTFÄLT                        
007500     03 IDHLOTAB-IN          PIC X(2).                                    
007600*                                 MFS BEHANDLING AV INPUTFÄLT             
007700     03 TEMFSINF             PIC X(55).                                   
007800*                                 INFORMATIONSMEDDELANDE                  
007900*** END COPY W4O36101    LENGTH=717                                       
