000100 01  MOD-W2O44201.                                                        
000200*                                 MOD-COPYTEXT FÖR W2044200               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(2).                                    
000800*                                 MFS BEHANDLING AV INPUTFÄLT             
000900     03 MOD-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-DASH             PIC X.                                       
001200     03 MOD-REKSIFFR         PIC 9.                                       
001300*                                 KONTROLLSIFFRA                          
001400     03 MOD-IDANSK-FOM-IN    PIC X(2).                                    
001500*                                 MFS BEHANDLING AV INPUTFÄLT             
001600     03 MOD-IDANSK-FOM-UT    PIC Z(2)9.                                   
001700*                                 ANSKAFFARNUMMER                         
001800     03 MOD-IDANSK-TOM-IN    PIC X(2).                                    
001900*                                 MFS BEHANDLING AV INPUTFÄLT             
002000     03 MOD-IDANSK-TOM-UT    PIC Z(2)9.                                   
002100*                                 ANSKAFFARNUMMER                         
002200     03 MOD-IDPROJ-IN        PIC X(2).                                    
002300*                                 MFS BEHANDLING AV INPUTFÄLT             
002400     03 MOD-IDPROJ-UT        PIC X(4).                                    
002500*                                 PARTS PROJEKTIDENTITET                  
002600     03 MOD-IDDC-IN          PIC X(2).                                    
002700*                                 MFS BEHANDLING AV INPUTFÄLT             
002800     03 MOD-IDDC-UT          PIC X(2).                                    
002900*                                 IDENTIFIERARE LAGER                     
003000     03 MOD-IDLEVNR-DC-UT    PIC X(5).                                    
003100*                                 DC LEVERANTÖR                           
003200     03 MOD-INFO-RAD         OCCURS 12 TIMES.                             
003300*                                 RADINFORMATION                          
003400        05 MOD-IDARTNR       PIC Z(9).                                    
003500*                                 ARTIKELNUMMER                           
003600        05 MOD-IDPROJ        PIC X(4).                                    
003700*                                 PARTS PROJEKTIDENTITET                  
003800        05 MOD-IDPROJK       PIC X(4).                                    
003900*                                 PROJEKTIDENTITET KONSTRUKTION           
004000        05 MOD-IDPROJUP      PIC X(8).                                    
004100*                                 PROJEKTUPPDRAG                          
004200        05 MOD-IDLEVNR-DC    PIC X(5).                                    
004300*                                 DC LEVERANTÖR                           
004400        05 MOD-DAPUBL        PIC 9(4).                                    
004500*                                 ÅR - VECKA  (ÅÅVV)                      
004600        05 MOD-FLPISK        PIC X.                                       
004700*                                 PISK ARTIKEL                            
004800        05 MOD-TIINKOP       PIC 9(4).                                    
004900*                                 ÅR - VECKA  (ÅÅVV)                      
005000        05 MOD-IDINK         PIC X(4).                                    
005100*                                 INKÖPARNUMMER                           
005200        05 MOD-TIMOTSI       PIC 9(6).                                    
005300*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
005400     03 MOD-TEMFSINF         PIC X(55).                                   
005500*                                 INFORMATIONSMEDDELANDE                  
005600*** END OF VILMAII-COPY LENGTH= 725 BYTES                                 
