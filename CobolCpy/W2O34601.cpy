000100 01  MOD-W2O34601.                                                        
000200*                                 MODCOPYTEXT TILL BILD 2346              
000300*                                 REFILLTABELL                            
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDDC-IN          PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000     03 MOD-IDDC-UT          PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200     03 MOD-IDREFTAB-IN      PIC X.                                       
001300*                                 IDENTITET REFILLTABELL                  
001400     03 MOD-IDREFTAB-UT      PIC X.                                       
001500*                                 IDENTITET REFILLTABELL                  
001600     03 MOD-IDREFTAB-FIRST   PIC X.                                       
001700*                                 IDENTITET REFILLTABELL                  
001800     03 MOD-IDREFTAB-LAST    PIC X.                                       
001900*                                 IDENTITET REFILLTABELL                  
002000     03 MOD-KDCMD-IN-ATTR    PIC X(2).                                    
002100*                                 MFS ATTRIBUTFÄLT                        
002200     03 MOD-KDCMD-IN         PIC X(2).                                    
002300*                                 MFS BEHANDLING AV INPUTFÄLT             
002400     03 MOD-COPY-DATA.                                                    
002500*                                 NAMN PÅ TABELL ATT KOPIERA              
002600*                                                                         
002700        05 MOD-COPY-IDDC-IN-ATTR                                          
002800                             PIC X(2).                                    
002900*                                 MFS ATTRIBUTFÄLT                        
003000        05 MOD-COPY-IDDC-IN  PIC X(2).                                    
003100*                                 MFS BEHANDLING AV INPUTFÄLT             
003200        05 MOD-COPY-IDREFTAB-IN-ATTR                                      
003300                             PIC X(2).                                    
003400*                                 MFS ATTRIBUTFÄLT                        
003500        05 MOD-COPY-IDREFTAB-IN                                           
003600                             PIC X(2).                                    
003700*                                 MFS BEHANDLING AV INPUTFÄLT             
003800     03 MOD-TEREFLIM-ATTR    PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000     03 MOD-TEREFLIM         PIC X(70).                                   
004100*                                 REFILLORDERFÖRSLAGTAB. GRÄNS            
004200     03 MOD-PB-RAD-UT.                                                    
004300*                                 VISAR PB FÖR RESP KOLUMN                
004400*                                                                         
004500        05 MOD-KVPB-REF      OCCURS 8 TIMES                               
004600                             PIC Z(4)9.9.                                 
004700*                                 PERIODBEHOV REFILLING                   
004800     03 MOD-PB-RAD-IN.                                                    
004900*                                 UPPDATERING PB RAD                      
005000*                                                                         
005100        05 MOD-PB-GRUPP-IN   OCCURS 8 TIMES.                              
005200*                                 UTFÄLT UPPDATERING PB-RAD               
005300*                                                                         
005400           07 MOD-KVPB-REF-IN-ATTR                                        
005500                             PIC X(2).                                    
005600*                                 MFS ATTRIBUTFÄLT                        
005700           07 MOD-KVPB-REF-IN                                             
005800                             PIC X(2).                                    
005900*                                 MFS BEHANDLING AV INPUTFÄLT             
006000     03 MOD-PRISRADER.                                                    
006100*                                 VISNINGSRADER REFILLPUNKTER             
006200*                                                                         
006300        05 MOD-PRISRAD       OCCURS 10 TIMES.                             
006400*                                 REFILLPUNKTER PER PRIS                  
006500*                                                                         
006600           07 MOD-PRARTBES   PIC Z(6)9.9(2).                              
006700*                                 BESTÄLLNINGSPRIS I KRONOR               
006800           07 MOD-REFILLPUNKT                                             
006900                             OCCURS 8 TIMES.                              
007000*                                 REFILLPUNKT                             
007100*                                                                         
007200              09 MOD-KVREFLIM                                             
007300                             PIC Z(4)9.                                   
007400*                                 FAKTOR FÖR REFILLPUNKT.                 
007500              09 MOD-KDREFPKT-LIM                                         
007600                             PIC X.                                       
007700*                                 TYP AV FAKTOR FÖR REFILLPKT             
007800     03 MOD-PUNKTRAD-IN.                                                  
007900*                                 INMATNINGSRAD REFILLPUNKTER             
008000*                                 31 POS / RAD                            
008100        05 MOD-RAD-IN-ATTR   PIC X(2).                                    
008200*                                 MFS ATTRIBUTFÄLT                        
008300        05 MOD-RAD-IN        PIC X(2).                                    
008400*                                 MFS BEHANDLING AV INPUTFÄLT             
008500        05 MOD-PRARTBES-IN-ATTR                                           
008600                             PIC X(2).                                    
008700*                                 MFS ATTRIBUTFÄLT                        
008800        05 MOD-PRARTBES-IN   PIC X(2).                                    
008900*                                 MFS BEHANDLING AV INPUTFÄLT             
009000        05 MOD-PUNKTGRUPP-IN OCCURS 8 TIMES.                              
009100*                                 INMATNING PUNKTGRUPP                    
009200*                                                                         
009300           07 MOD-KVREFLIM-IN-ATTR                                        
009400                             PIC X(2).                                    
009500*                                 MFS ATTRIBUTFÄLT                        
009600           07 MOD-KVREFLIM-IN                                             
009700                             PIC X(2).                                    
009800*                                 MFS BEHANDLING AV INPUTFÄLT             
009900           07 MOD-KDREFPKT-LIM-IN-ATTR                                    
010000                             PIC X(2).                                    
010100*                                 MFS ATTRIBUTFÄLT                        
010200           07 MOD-KDREFPKT-LIM-IN                                         
010300                             PIC X(2).                                    
010400*                                 MFS BEHANDLING AV INPUTFÄLT             
010500     03 MOD-TEMFSINF         PIC X(55).                                   
010600*                                 INFORMATIONSMEDDELANDE                  
010700*** END OF VILMAII-COPY LENGTH= 931 BYTES                                 
