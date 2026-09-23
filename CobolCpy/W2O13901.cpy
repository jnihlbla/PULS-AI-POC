000100 01  W2O13901.                                                            
000200*                                 COPYTEXT FÖR MOD W2O13901               
000300     03 IDTRANS              PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MESSAGE              PIC X(40).                                   
000600*                                 MEDDELANDEFÄLT PÅ RAD 1                 
000700     03 IDARTNR-IN-ATTR      PIC X(2).                                    
000800*                                 MFS ATTRIBUTFÄLT                        
000900     03 IDARTNR-IN           PIC X(2).                                    
001000*                                 MFS BEHANDLING AV INPUTFÄLT             
001100     03 KDBEHX-PLAN-IN-ATTR  PIC X(2).                                    
001200*                                 MFS ATTRIBUTFÄLT                        
001300     03 KDBEHX-PLAN-IN       PIC X.                                       
001400*                                 BEHANDLINGSKOD-X                        
001500     03 KDBEHX-PLAN-UT       PIC X.                                       
001600*                                 BEHANDLINGSKOD-X                        
001700     03 IDLEVNR-IN-ATTR      PIC X(2).                                    
001800*                                 MFS ATTRIBUTFÄLT                        
001900     03 IDLEVNR-IN           PIC X(2).                                    
002000*                                 MFS BEHANDLING AV INPUTFÄLT             
002100     03 PERIOD-IN-ATTR       PIC X(2).                                    
002200*                                 MFS ATTRIBUTFÄLT                        
002300     03 PERIOD-IN            PIC X(2).                                    
002400*                                 MFS BEHANDLING AV INPUTFÄLT             
002500     03 IDARTNR.                                                          
002600        05 IDARTNR-UT        PIC X(9).                                    
002700*                                 ARTIKELNUMMER                           
002800        05 STRECK-1          PIC X.                                       
002900        05 REKSIFFR          PIC X.                                       
003000*                                 KONTROLLSIFFRA                          
003100     03 IDLEVNR-UT           PIC X(5).                                    
003200*                                 LEVERANTÖRNUMMER                        
003300     03 IDLEVNR-SHIP-UT      PIC X(5).                                    
003400*                                 SKEPPANDE LEVERANTÖR                    
003500     03 PERIOD-UT            PIC X(4).                                    
003600*                                 ÅR - REDOVISNINGSPERIOD (ÅÅRP)          
003700*                                 12 PER ÅR                               
003800     03 AREA-OUTPUT.                                                      
003900*                                                                         
004000        05 BEART-SVE         PIC X(25).                                   
004100*                                 SVENSK ARTIKELBENÄMNING                 
004200        05 AVS-DAG           OCCURS 5 TIMES                               
004300                             PIC X(2).                                    
004400        05 IDANSK            PIC Z(2)9.                                   
004500*                                 ANSKAFFARNUMMER                         
004600        05 KVVECKOR-LT       PIC Z(2).                                    
004700*                                 ANTAL VECKOR LEDTID                     
004800        05 KVVECKOR-FT       PIC Z(2).                                    
004900*                                 ANTAL VECKOR FRYSNINGSTID               
005000        05 KVVECKOR-BT       PIC Z(2).                                    
005100*                                 ANTAL VECKOR BESTÄLLNINGSTID            
005200        05 KDLPSP            PIC Z.                                       
005300*                                 LEVERANSPLANESPÄRR                      
005400        05 TILPSP            PIC X(4).                                    
005500*                                 DATUM LEVERANSPLAN-SPÄRR (ÅÅVV)         
005600        05 KVBR              PIC Z(7).                                    
005700*                                 BESTÄLLNINGSREST                        
005800        05 PERIODER          OCCURS 2 TIMES.                              
005900*                                 AVROPSTABELL   NYA AVROP                
006000           07 PERIOD-AARP    PIC 9(4).                                    
006100*                                 ÅR - REDOVISNINGSPERIOD (ÅÅRP)          
006200*                                 12 PER ÅR                               
006300           07 VECKOR         OCCURS 5 TIMES.                              
006400              09 KVAVROP-TAB PIC X(6).                                    
006500              09 SKILJETECKEN-TAB                                         
006600                             PIC X.                                       
006700              09 TIAVROP-AVS-TAB                                          
006800                             PIC Z(2).                                    
006900*                                                 TIAVROP-AVS-002         
007000*                                 AVSÄNDNINGSDATUM (VV)                   
007100              09 DAGAR       OCCURS 5 TIMES.                              
007200                 11 KVAVROP-DAG-UT                                        
007300                             PIC Z(5)9.                                   
007400                 11 KVAVROP-DAG-IN-ATTR                                   
007500                             PIC X(2).                                    
007600*                                 MFS ATTRIBUTFÄLT                        
007700                 11 KVAVROP-DAG-IN                                        
007800                             PIC Z(5)9.                                   
007900     03 LINE23.                                                           
008000        05 MESSAGE-BOTTOM    PIC X(56).                                   
008100*** END OF VILMAII-COPY LENGTH= 995 BYTES                                 
