000100 01  MOD-W6O17501.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD 6175              
000300*                                 PRIME COUNT SELECTION                   
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDLEVNR-IN       PIC X(2).                                    
000900*                                 MFS BEHANDLING AV INPUTFÄLT             
001000     03 MOD-IDLEVNR-UT       PIC X(5).                                    
001100*                                 LEVERANTÖRNUMMER                        
001200     03 MOD-TISUPREF-IN      PIC X(2).                                    
001300*                                 MFS BEHANDLING AV INPUTFÄLT             
001400     03 MOD-TISUPREF-UT      PIC X(6).                                    
001500*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
001600     03 MOD-IDSUPREF-IN      PIC X(2).                                    
001700*                                 MFS BEHANDLING AV INPUTFÄLT             
001800     03 MOD-IDSUPREF-UT      PIC X(10).                                   
001900*                                 LEVERANTöRSREF.                         
002000     03 MOD-IDDISTR-IN       PIC X(2).                                    
002100*                                 MFS BEHANDLING AV INPUTFÄLT             
002200     03 MOD-IDDISTR-UT       PIC X(4).                                    
002300*                                 DISTRIKTNUMMER                          
002400     03 MOD-TABELLRAD        OCCURS 15 TIMES.                             
002500*                                 GRUPP MED TABELL RADER                  
002600        05 MOD-CMD-ATTR      PIC X(2).                                    
002700*                                 MFS ATTRIBUTFÄLT                        
002800        05 MOD-CMD           PIC X.                                       
002900        05 MOD-TISUPREF      PIC 9(6).                                    
003000*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
003100        05 MOD-TISUPTID      PIC Z9.9(2).                                 
003200*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
003300        05 MOD-IDSUPREF      PIC X(10).                                   
003400*                                 LEVERANTöRSREF.                         
003500        05 MOD-TIPACKN       PIC 9(6).                                    
003600*                                 PACKNINGSDATUM         (ÅÅMMDD)         
003700        05 MOD-TIPACTID      PIC Z9.9(2).                                 
003800*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
003900        05 MOD-IDDISTR       PIC Z(3)9.                                   
004000*                                 DISTRIKTNUMMER                          
004100        05 MOD-IDKUNDNR      PIC Z(5)9.                                   
004200*                                 KUNDNUMMER                              
004300        05 MOD-IDORDNR5      PIC Z(4)9.                                   
004400*                                 ORDERNUMMER                             
004500        05 MOD-IDKOLLI       PIC Z(4)9.                                   
004600*                                 KOLLINUMMER                             
004700        05 MOD-VKORDBTO-KOLLI                                             
004800                             PIC Z(5)9.9.                                 
004900*                                 ORDERVIKT BRUTTO PER KOLLI              
005000        05 MOD-VLORDBTO-KOLLI                                             
005100                             PIC Z(2)9.9(3).                              
005200*                                 ORDERVOLYM BRUTTO KOLLI                 
005300     03 MOD-TEMFSINF         PIC X(55).                                   
005400*                                 INFORMATIONSMEDDELANDE                  
005500*** END OF VILMAII-COPY LENGTH= 1182 BYTES                                
