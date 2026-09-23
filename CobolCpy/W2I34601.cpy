000100 01  MID-W2I34601.                                                        
000200*                                 MIDCOPYTEXT FÖR BILD 2346               
000300*                                 REFILLTABELL                            
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
001600     03 MID-KDCMD            PIC X.                                       
001700      88 MID-KDCMD-INGENTING VALUE ' '.                                   
001800      88 MID-KDCMD-DELETE    VALUE 'D'                                    
001900                             'B'.                                         
002000      88 MID-KDCMD-REPLACE   VALUE 'R'                                    
002100                             'Ä'.                                         
002200      88 MID-KDCMD-INSERT    VALUE 'I'                                    
002300                             'N'.                                         
002400*                                 RAD-UPPDATERINGSKOMMANDO                
002500     03 MID-COPY-DATA.                                                    
002600*                                 DATA FÖR KOPIERING AV TABELL            
002700*                                                                         
002800        05 MID-COPY-IDDC     PIC X(2).                                    
002900*                                 IDENTIFIERARE LAGER                     
003000        05 MID-COPY-IDREFTAB PIC X.                                       
003100*                                 IDENTITET REFILLTABELL                  
003200     03 MID-INPUT.                                                        
003300*                                 DATA INMATNINGSFÄLT 2346                
003400*                                                                         
003500        05 MID-TEREFLIM      PIC X(70).                                   
003600*                                 REFILLORDERFÖRSLAGTAB. GRÄNS            
003700        05 MID-PB-RAD.                                                    
003800*                                 INMATNINGSRAD PB                        
003900*                                                                         
004000           07 MID-KVPB-REF   OCCURS 8 TIMES                               
004100                             PIC X(7).                                    
004200*                                 PERIODBEHOV (PROGNOS)                   
004300        05 MID-PRISRAD.                                                   
004400*                                 INDATARAD PUNKTER                       
004500*                                                                         
004600           07 MID-RAD        PIC X.                                       
004700           07 MID-PRARTBES   PIC X(10).                                   
004800*                                 BESTÄLLNINGSPRIS I KRONOR               
004900           07 MID-PROGNOS-KOLUMN                                          
005000                             OCCURS 8 TIMES.                              
005100*                                 PÅFYLLNADSPUNKT PER PROGNOS/            
005200*                                 KOLUMN                                  
005300              09 MID-KVREFLIM                                             
005400                             PIC X(5).                                    
005500*                                 FAKTOR FÖR REFILLPUNKT.                 
005600              09 MID-KDREFPKT-LIM                                         
005700                             PIC X.                                       
005800*                                 TYP AV FAKTOR FÖR REFILLPKT             
005900*** END OF VILMAII-COPY LENGTH= 197 BYTES                                 
