000100 01  MOD-W4O51501.                                                        
000200*                                 MOD-COPYTEXT                            
000300*                                 FÖR W40515                              
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDDISTR-IN       PIC X(2).                                    
000900*                                 MFS BEHANDLING AV INPUTFÄLT             
001000     03 MOD-IDDISTR-UT       PIC X(4).                                    
001100*                                 DISTRIKTNUMMER                          
001200     03 MOD-IDKUNDNR-IN      PIC X(2).                                    
001300*                                 MFS BEHANDLING AV INPUTFÄLT             
001400     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001500*                                 KUNDNUMMER                              
001600     03 MOD-IDDC-IN          PIC X(2).                                    
001700*                                 IDENTIFIERARE LAGER                     
001800     03 MOD-IDDC-UT          PIC X(2).                                    
001900*                                 IDENTIFIERARE LAGER                     
002000     03 MOD-KDORDKL-IN       PIC X.                                       
002100*                                 ORDERKLASS                              
002200     03 MOD-KDORDKL-UT       PIC X.                                       
002300*                                 ORDERKLASS                              
002400     03 MOD-KDFRAKT-IN       PIC X(2).                                    
002500*                                 MFS BEHANDLING AV INPUTFÄLT             
002600     03 MOD-KDFRAKT-UT       PIC X(2).                                    
002700*                                 FRAKTSÄTT DC TILL KUND                  
002800     03 MOD-KDORDSTA-IN      PIC X.                                       
002900*                                 VOLVOORDERSTATUS                        
003000     03 MOD-KDORDSTA-UT      PIC X.                                       
003100*                                 VOLVOORDERSTATUS                        
320000     03 MOD-IDTRANS-RAD      OCCURS 14 TIMES                              
330000                             PIC X(4).                                    
340000*                                 BILDNUMMER                              
350000     03 MOD-IDKUNDNR         OCCURS 14 TIMES                              
360000                             PIC Z(5)9.                                   
370000*                                 KUNDNUMMER                              
380000     03 MOD-IDORDNR7         OCCURS 14 TIMES                              
390000                             PIC Z(6)9.                                   
400000*                                 ORDERNUMMER                             
410000     03 MOD-IDDC             OCCURS 14 TIMES                              
420000                             PIC X(2).                                    
430000*                                 IDENTIFIERARE LAGER                     
440000     03 MOD-KDFRAKT          OCCURS 14 TIMES                              
450000                             PIC Z9.                                      
004600*                                 FRAKTSÄTT DC TILL KUND                  
620001     03 MOD-KDORDKL          OCCURS 14 TIMES                              
620002                             PIC 9.                                       
620003*                                 ORDERKLASS                              
620004     03 MOD-KDORDSTA         OCCURS 14 TIMES                              
620005                             PIC X(2).                                    
620006*                                 VOLVOORDERSTATUS                        
620007     03 MOD-KVKOLPAC         OCCURS 14 TIMES                              
620008                             PIC Z(3)9.                                   
620009*                                 ANTAL PACK RAPPORTERADE KOLLI           
620010     03 MOD-KVKOLLI-FAKT     OCCURS 14 TIMES                              
620011                             PIC Z(3)9.                                   
620012*                                 ANTAL FAKTURERADE KOLLIN                
620013     03 MOD-KVKOLLI-LAST     OCCURS 14 TIMES                              
620014                             PIC Z(3)9.                                   
620015*                                 ANTAL LASTNINGSRAPPORTERADE             
620016*                                 KOLLIN                                  
630000     03 MOD-VKORDBTO         OCCURS 14 TIMES                              
640000                             PIC Z(5)9.9.                                 
650000*                                 ORDERVIKT BRUTTO (KG)                   
660000     03 MOD-VLORDBTO         OCCURS 14 TIMES                              
670000                             PIC Z(3)9.9(3).                              
680000*                                 ORDERVOLYM BRUTTO (M3)                  
690000     03 MOD-TEMFSINF         PIC X(55).                                   
700000*                                 INFORMATIONSMEDDELANDE                  
007100*** END OF VILMAII-COPY LENGTH= 853 BYTES                                 
