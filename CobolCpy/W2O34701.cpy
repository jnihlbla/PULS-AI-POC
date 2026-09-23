000100 01  MOD-W2O34701.                                                        
000200*                                 MODCOPYTEXT TILL BILD 2347              
000300*                                 REFILLTABELL, KVANTER                   
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
002000     03 MOD-TEREFLIM-ATTR    PIC X(2).                                    
002100*                                 MFS ATTRIBUTFÄLT                        
002200     03 MOD-TEREFLIM         PIC X(70).                                   
002300*                                 REFILLORDERFÖRSLAGTAB. GRÄNS            
002400     03 MOD-PB-RAD-UT.                                                    
002500*                                 VISAR PB FÖR RESP KOLUMN                
002600*                                                                         
002700        05 MOD-KVPB-REF      OCCURS 8 TIMES                               
002800                             PIC Z(4)9.9.                                 
002900*                                 PERIODBEHOV REFILLING                   
003000     03 MOD-PRISRADER.                                                    
003100*                                 VISNINGSRADER REFILLKVANTER             
003200*                                                                         
003300        05 MOD-PRISRAD       OCCURS 10 TIMES.                             
003400*                                 REFILLKVANTER PER PRIS                  
003500*                                                                         
003600           07 MOD-PRARTBES   PIC Z(6)9.9(2).                              
003700*                                 BESTÄLLNINGSPRIS I KRONOR               
003800           07 MOD-REFILLKVANT                                             
003900                             OCCURS 8 TIMES.                              
004000*                                 REFILLKVANT                             
004100*                                                                         
004200              09 MOD-KVREFKVA                                             
004300                             PIC Z(4)9.                                   
004400*                                 FAKTOR FÖR REFILLKVANTITET.             
004500              09 MOD-KDREFPKT-KVA                                         
004600                             PIC X.                                       
004700*                                 TYP AV FAKTOR FÖR REFILLKVANT           
004800     03 MOD-KVANTRAD-IN.                                                  
004900*                                 INMATNINGSRAD REFILLKVANTER             
005000*                                 31 POS / RAD                            
005100        05 MOD-RAD-IN-ATTR   PIC X(2).                                    
005200*                                 MFS ATTRIBUTFÄLT                        
005300        05 MOD-RAD-IN        PIC X(2).                                    
005400*                                 MFS BEHANDLING AV INPUTFÄLT             
005500        05 MOD-KVANTGRUPP-IN OCCURS 8 TIMES.                              
005600*                                 INMATNING KVANTGRUPP                    
005700*                                                                         
005800           07 MOD-KVREFKVA-IN-ATTR                                        
005900                             PIC X(2).                                    
006000*                                 MFS ATTRIBUTFÄLT                        
006100           07 MOD-KVREFKVA-IN                                             
006200                             PIC X(2).                                    
006300*                                 MFS BEHANDLING AV INPUTFÄLT             
006400           07 MOD-KDREFPKT-KVA-IN-ATTR                                    
006500                             PIC X(2).                                    
006600*                                 MFS ATTRIBUTFÄLT                        
006700           07 MOD-KDREFPKT-KVA-IN                                         
006800                             PIC X(2).                                    
006900*                                 MFS BEHANDLING AV INPUTFÄLT             
007000     03 MOD-TEMFSINF         PIC X(55).                                   
007100*                                 INFORMATIONSMEDDELANDE                  
007200*** END OF VILMAII-COPY LENGTH= 883 BYTES                                 
