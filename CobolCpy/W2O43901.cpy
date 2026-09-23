000100 01  W2O43901.                                                            
000200*                                 COPYTEXT FÖR MOD W2O43901               
000300     03 IDTRANS              PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 TEMFSFEL             PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 IDARTNR-IN-ATTR      PIC X(2).                                    
000800*                                 MFS ATTRIBUTFÄLT                        
000900     03 IDARTNR-IN           PIC X(2).                                    
001000*                                 MFS BEHANDLING AV INPUTFÄLT             
001100     03 IDDC-IN-ATTR         PIC X(2).                                    
001200*                                 MFS ATTRIBUTFÄLT                        
001300     03 IDDC-IN              PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 KDBEHX-PLAN-IN-ATTR  PIC X(2).                                    
001600*                                 MFS ATTRIBUTFÄLT                        
001700     03 KDBEHX-PLAN-IN       PIC X.                                       
001800*                                 BEHANDLINGSKOD-X                        
001900     03 IDLEVNR-IN-ATTR      PIC X(2).                                    
002000*                                 MFS ATTRIBUTFÄLT                        
002100     03 IDLEVNR-IN           PIC X(2).                                    
002200*                                 MFS BEHANDLING AV INPUTFÄLT             
002300     03 PERIOD-IN-ATTR       PIC X(2).                                    
002400*                                 MFS ATTRIBUTFÄLT                        
002500     03 PERIOD-IN            PIC X(2).                                    
002600*                                 MFS BEHANDLING AV INPUTFÄLT             
002700     03 IDARTNR.                                                          
002800        05 IDARTNR-UT        PIC X(9).                                    
002900*                                 ARTIKELNUMMER                           
003000        05 DASH-1            PIC X.                                       
003100        05 REKSIFFR          PIC X.                                       
003200*                                 KONTROLLSIFFRA                          
003300     03 IDDC-UT              PIC X(2).                                    
003400*                                 IDENTIFIERARE LAGER                     
003500     03 KDBEHX-PLAN-UT       PIC X.                                       
003600*                                 BEHANDLINGSKOD-X                        
003700     03 IDLEVNR-UT           PIC X(5).                                    
003800*                                 LEVERANTÖRNUMMER                        
003900     03 IDLEVNR-SHIP-UT      PIC X(5).                                    
004000*                                 SKEPPANDE LEVERANTÖR                    
004100     03 PERIOD-UT            PIC X(4).                                    
004200*                                 ÅR - REDOVISNINGSPERIOD (ÅÅRP)          
004300*                                 12 PER ÅR (OCKSÅ LOGISTIKPER)           
004400     03 AREA-OUTPUT.                                                      
004500*                                                                         
004600        05 BEART-ENG         PIC X(25).                                   
004700*                                 ENGELSK ARTIKELBENÄMNING                
004800        05 DISP-DAY          OCCURS 5 TIMES                               
004900                             PIC X(2).                                    
005000        05 IDANSK            PIC Z(2)9.                                   
005100*                                 ANSKAFFARNUMMER                         
005200        05 KVVECKOR-LT       PIC Z(2).                                    
005300*                                 ANTAL VECKOR LEDTID                     
005400        05 KDLPSP            PIC Z.                                       
005500*                                 LEVERANSPLANESPÄRR                      
005600        05 TILPSP            PIC X(4).                                    
005700*                                 DATUM LEVERANSPLAN-SPÄRR (ÅÅVV)         
005800        05 PERIOD-AREA       OCCURS 2 TIMES.                              
005900*                                 AVROPSTABELL   NYA AVROP                
006000           07 PERIOD-AARP    PIC 9(4).                                    
006100*                                 ÅR - REDOVISNINGSPERIOD (ÅÅRP)          
006200*                                 12 PER ÅR (OCKSÅ LOGISTIKPER)           
006300           07 WEEK-AREA      OCCURS 5 TIMES.                              
006400              09 KVAVROP-TAB PIC X(6).                                    
006500              09 SIGN-TAB    PIC X.                                       
006600              09 TIAVROP-AVS-TAB                                          
006700                             PIC Z(2).                                    
006800*                                                 TIAVROP-AVS-002         
006900*                                 AVSÄNDNINGSDATUM (VV)                   
007000              09 DAYS        OCCURS 5 TIMES.                              
007100                 11 KVAVROP-DAY-UT                                        
007200                             PIC Z(5)9.                                   
007300                 11 KVAVROP-DAY-IN-ATTR                                   
007400                             PIC X(2).                                    
007500*                                 MFS ATTRIBUTFÄLT                        
007600                 11 KVAVROP-DAY-IN                                        
007700                             PIC Z(5)9.                                   
007800     03 TEMFSINF             PIC X(55).                                   
007900*                                 INFORMATIONSMEDDELANDE                  
008000*** END OF VILMAII-COPY LENGTH= 989 BYTES                                 
