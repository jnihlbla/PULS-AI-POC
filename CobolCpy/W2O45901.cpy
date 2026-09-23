000100 01  MOD-W2O45901-CTX.                                                    
000200*                                 COPYTEXT FÖR MOD W2I45901               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDDC-IN          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MOD-IDARTNR-UT       PIC X(9).                                    
001200*                                 ARTIKELNUMMER                           
001300     03 MOD-IDDC-UT          PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 MOD-BEART-ENG        PIC X(25).                                   
001600*                                 ENGELSK ARTIKELBENÄMNING                
001700     03 MOD-KVLS             PIC Z(6)9.                                   
001800*                                 LAGERSALDO                              
001900     03 MOD-KVOKS            PIC -(6)9.                                   
002000*                                 ORDERKÖSALDO                            
002100     03 MOD-KVDISP           PIC -(7)9.                                   
002200*                                 DISPONIBELT LAGER                       
002300     03 MOD-KVAKS            PIC Z(6)9.                                   
002400*                                 ANKOMSTSALDO                            
002500     03 MOD-KVSPANT          PIC Z(6)9.                                   
002600*                                 SPÄRRAT ANTAL                           
002700     03 MOD-KVSLUTKP         PIC Z(6)9.                                   
002800*                                 SLUTKÖPSSALDO                           
002900     03 MOD-TISLUTKP         PIC Z(6).                                    
003000*                                 DÅ SLUTKÖPSDATUM BÖRJAR GÄLLA           
003100     03 MOD-KDERS-ATTR       PIC X(2).                                    
003200*                                 MFS ATTRIBUTFÄLT                        
003300     03 MOD-KDERS            PIC Z9.                                      
003400*                                 ERSÄTTNINGSKOD                          
003500     03 MOD-FLERSATT         PIC X.                                       
003600*                                 ERSATT I VIPS                           
003700     03 MOD-FLSKROT-BEORD    PIC X.                                       
003800*                                 SKROTNING BEORDRAD AV ANSK              
003900     03 MOD-TISKROT-AUTO-ATTR                                             
004000                             PIC X(2).                                    
004100*                                 MFS ATTRIBUTFÄLT                        
004200     03 MOD-TISKROT-AUTO     PIC X(6).                                    
004300*                                 STOPDATE AUTO-SKROTNING                 
004400     03 MOD-KVSKROT-KVAR-ATTR                                             
004500                             PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700     03 MOD-KVSKROT-KVAR     PIC X(7).                                    
004800*                                 KVARLIGGANDE ANTAL                      
004900     03 MOD-SUSKROT          PIC Z(8)9.                                   
005000*                                 SUMMA SKROTAT ANTAL                     
005100*                                 AV 1 ARTIKEL                            
005200     03 MOD-SKROT-TEXT-ATTR  PIC X(2).                                    
005300*                                 MFS ATTRIBUTFÄLT                        
005400     03 MOD-SKROT-TEXT       PIC X(20).                                   
005500     03 MOD-IDKONTO-ATTR     PIC X(2).                                    
005600*                                 MFS ATTRIBUTFÄLT                        
005700     03 MOD-IDKONTO          PIC Z(9)9.                                   
005800*                                 KONTO                                   
005900     03 MOD-IDANALYS-ATTR    PIC X(2).                                    
006000*                                 MFS ATTRIBUTFÄLT                        
006100     03 MOD-IDANALYS         PIC X(12).                                   
006200*                                 ANALYSNUMMER                            
006300     03 MOD-KVSKROT          PIC Z(6)9.                                   
006400*                                 ANTAL SENASTE SKROTORDER                
006500     03 MOD-TISKROT          PIC Z(6).                                    
006600*                                 SKROTNINGSDATUM                         
006700     03 MOD-TISKROT-BEORD    PIC Z(6).                                    
006800*                                 BEORDRAD SKROTNINGSDATUM                
006900     03 MOD-TEMFSINF         PIC X(55).                                   
007000*                                 INFORMATIONSMEDDELANDE                  
007100*** END OF VILMAII-COPY LENGTH= 294 BYTES                                 
