000100 01  MID-W2I45601.                                                        
000200*                                 MIDCOPYTEXT FÖR BILD 2456               
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
002300                             'N'                                          
002400                             'A'.                                         
002500      88 MID-KDCMD-SELECT    VALUE 'S'                                    
002600                             'V'.                                         
002700      88 MID-KDCMD-PRINT     VALUE 'P'                                    
002800                             'P'.                                         
002900      88 MID-KDCMD-COPY      VALUE 'C'                                    
003000                             'K'.                                         
003100*                                 RAD-UPPDATERINGSKOMMANDO                
003200*                                  BLANK  = INGENTING                     
003300*                                  D , B  = DELETE                        
003400*                                  R , Ä  = REPLACE                       
003500*                                  I,N,A  = INSERT                        
003600*                                  S , V  = SELECT                        
003700*                                  P , P  = PRINT                         
003800*                                  C , K  = COPY                          
003900     03 MID-COPY-DATA.                                                    
004000*                                 DATA FÖR KOPIERING AV TABELL            
004100*                                                                         
004200        05 MID-COPY-IDDC     PIC X(2).                                    
004300*                                 IDENTIFIERARE LAGER                     
004400        05 MID-COPY-IDREFTAB PIC X.                                       
004500*                                 IDENTITET REFILLTABELL                  
004600     03 MID-INPUT.                                                        
004700*                                 DATA INMATNINGSFÄLT 2456                
004800*                                                                         
004900        05 MID-TEREFLIM      PIC X(70).                                   
005000*                                 REFILLORDERFÖRSLAGTAB. GRÄNS            
005100        05 MID-PB-RAD.                                                    
005200*                                 INMATNINGSRAD PB                        
005300*                                                                         
005400           07 MID-KVPB-REF   OCCURS 8 TIMES                               
005500                             PIC X(7).                                    
005600*                                 PERIODBEHOV (PROGNOS)                   
005700        05 MID-PRISRAD.                                                   
005800*                                 INDATARAD PUNKTER                       
005900*                                                                         
006000           07 MID-RAD        PIC X.                                       
006100           07 MID-PRARTBES   PIC X(10).                                   
006200*                                 BESTÄLLNINGSPRIS I KRONOR               
006300           07 MID-PROGNOS-KOLUMN                                          
006400                             OCCURS 8 TIMES.                              
006500*                                 PÅFYLLNADSPUNKT PER PROGNOS/            
006600*                                 KOLUMN                                  
006700              09 MID-KVREFLIM                                             
006800                             PIC X(5).                                    
006900*                                 FAKTOR FÖR REFILLPUNKT.                 
007000              09 MID-KDREFPKT-LIM                                         
007100                             PIC X.                                       
007200*                                 TYP AV FAKTOR FÖR REFILLPKT             
007300*** END OF VILMAII-COPY LENGTH= 197 BYTES                                 
