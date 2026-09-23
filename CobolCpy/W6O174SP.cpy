000100 01  MOD-W6O17401.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD 6174              
000300*                                 PRIME COUNT SELECTION                   
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDLEVNR-IN       PIC X(2).                                    
000900*                                 MFS BEHANDLING AV INPUTFÄLT             
001000     03 MOD-IDLEVNR-UT       PIC Z(4)9.                                   
001100*                                 LEVERANTÖRNUMMER                        
001200     03 MOD-DASUPREF-IN      PIC X(2).                                    
001300*                                 MFS BEHANDLING AV INPUTFÄLT             
001400     03 MOD-DASUPREF-UT      PIC 9(6).                                    
001500*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
001600     03 MOD-IDSUPREF-IN      PIC X(2).                                    
001700*                                 MFS BEHANDLING AV INPUTFÄLT             
001800     03 MOD-IDSUPREF-UT      PIC X(10).                                   
001900*                                 LEVERANTöRSREF.                         
002000     03 MOD-TABELLRAD        OCCURS 15 TIMES.                             
002100*                                 GRUPP MED TABELL RADER                  
002200        05 MOD-CMD-ATTR      PIC X(2).                                    
002300*                                 MFS ATTRIBUTFÄLT                        
002400        05 MOD-CMD           PIC X.                                       
002500        05 MOD-DASUPREF      PIC 9(6).                                    
002600*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
002700        05 MOD-TISUPREF      PIC Z9.9(2).                                 
002800*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
002900        05 MOD-IDSUPREF      PIC X(10).                                   
003000*                                 LEVERANTöRSREF.                         
003100        05 MOD-KVKOLLI       PIC Z(4)9.                                   
003200*                                 ANTAL KOLLI                             
003300     03 MOD-TEMFSINF         PIC X(55).                                   
003400*                                 INFORMATIONSMEDDELANDE                  
003500*** END OF VILMAII-COPY LENGTH= 561 BYTES                                 
