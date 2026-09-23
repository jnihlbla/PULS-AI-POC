000100 01  MID-W2I45701.                                                        
000200*                                 MIDCOPYTEXT FÖR BILD 2457               
000300*                                 REFILLTABELL, KVANTER                   
000400     03 MID-IDDC-IN          PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600     03 MID-IDDC-UT          PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800     03 MID-IDREFTAB-IN      PIC X.                                       
000900*                                 IDENTITET REFILLTABELL                  
001000     03 MID-IDREFTAB-UT      PIC X.                                       
001100*                                 IDENTITET REFILLTABELL                  
001200     03 MID-IDREFTAB-FIRST   PIC X.                                       
001300*                                 IDENTITET REFILLTABELL                  
001400     03 MID-IDREFTAB-LAST    PIC X.                                       
001500*                                 IDENTITET REFILLTABELL                  
001600     03 MID-INPUT.                                                        
001700*                                 DATA INMATNINGSFÄLT 2457                
001800*                                                                         
001900        05 MID-TEREFLIM      PIC X(70).                                   
002000*                                 REFILLORDERFÖRSLAGTAB. GRÄNS            
002100        05 MID-PRISRAD.                                                   
002200*                                 INDATARAD KVANTER                       
002300*                                                                         
002400           07 MID-RAD        PIC X.                                       
002500           07 MID-PROGNOS-KOLUMN                                          
002600                             OCCURS 8 TIMES.                              
002700*                                 PÅFYLLNADSKVANT PER PROGNOS/            
002800*                                 KOLUMN                                  
002900              09 MID-KVREFKVA                                             
003000                             PIC X(5).                                    
003100*                                 FAKTOR FÖR REFILLKVANTITET.             
003200              09 MID-KDREFPKT-KVA                                         
003300                             PIC X.                                       
003400*                                 TYP AV FAKTOR FÖR REFILLKVANT           
003500*** END OF VILMAII-COPY LENGTH= 127 BYTES                                 
