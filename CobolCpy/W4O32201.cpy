000100 01  MOD-W4O32201.                                                        
000200*                                 MOD-COPYTEXT FÖR W4032200               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDANSTNR-IN      PIC X(2).                                    
000800*                                 MFS BEHANDLING AV INPUTFÄLT             
000900     03 MOD-IDANSTNR-UT      PIC X(5).                                    
001000*                                 ANSTÄLLNINGSNUMMER                      
001100     03 MOD-IDDISTR-IN       PIC X(2).                                    
001200*                                 MFS BEHANDLING AV INPUTFÄLT             
001300     03 MOD-IDDISTR-UT       PIC X(4).                                    
001400*                                 DISTRIKTNUMMER                          
001500     03 MOD-IDKUNDNR-IN      PIC X(2).                                    
001600*                                 MFS BEHANDLING AV INPUTFÄLT             
001700     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001800*                                 KUNDNUMMER                              
001900     03 MOD-IDORDNR-IN       PIC X(2).                                    
002000*                                 MFS BEHANDLING AV INPUTFÄLT             
002100     03 MOD-IDORDNR-UT       PIC X(5).                                    
002200*                                 ORDERNUMMER                             
002300     03 MOD-IDKOLLI-IN       PIC X(2).                                    
002400*                                 MFS BEHANDLING AV INPUTFÄLT             
002500     03 MOD-IDKOLLI-UT       PIC X(5).                                    
002600*                                 KOLLINUMMER                             
002700     03 MOD-IDPRODNR-IN      PIC X(2).                                    
002800*                                 MFS BEHANDLING AV INPUTFÄLT             
002900     03 MOD-IDPRODNR-UT      PIC X(7).                                    
003000*                                 PRODUKTIONSNUMMER                       
003100     03 MOD-IDDC-IN          PIC X(2).                                    
003200*                                 MFS BEHANDLING AV INPUTFÄLT             
003300     03 MOD-IDDC-UT          PIC X(2).                                    
003400*                                 IDENTIFIERARE LAGER                     
003500     03 MOD-IDDISTR          PIC Z(3)9.                                   
003600*                                 DISTRIKTNUMMER                          
003700     03 MOD-IDKUNDNR         PIC Z(5)9.                                   
003800*                                 KUNDNUMMER                              
003900     03 MOD-IDDC             PIC X(2).                                    
004000*                                 IDENTIFIERARE LAGER                     
004100     03 MOD-KDFRAKT          PIC Z9.                                      
004200*                                 FRAKTSÄTT DC TILL KUND                  
004300     03 MOD-IDKUNDRF         PIC X(10).                                   
004400*                                 KUNDENS REFERENS (ORDERID)              
004500     03 MOD-BEGMT-RAD1       PIC X(27).                                   
004600*                                 DEL AV GODSMOTTAGARNAMN                 
004700     03 MOD-UTSKRIFTSDATUM   PIC X(13).                                   
004800     03 MOD-BEGMT-RAD2       PIC X(27).                                   
004900*                                 DEL AV GODSMOTTAGARNAMN                 
005000     03 MOD-ADGMT-GATA       PIC X(27).                                   
005100*                                 DEL AV GODSMOTTAGARADRESS               
005200     03 MOD-BEVARREF         PIC X(10).                                   
005300*                                 VÅR REFERENS                            
005400     03 MOD-TIORDREG         PIC 9(6).                                    
005500*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
005600     03 MOD-KDORDKL          PIC 9.                                       
005700*                                 ORDERKLASS                              
005800     03 MOD-TIBEGPAC         PIC 9(6).                                    
005900*                                 BEGÄRD PACKNINGSDAG    (ÅÅMMDD)         
006000     03 MOD-ADGMT-PADR       PIC X(27).                                   
006100*                                 DEL AV GODSMOTTAGARADRESS               
006200     03 MOD-TIPACKN-SK       PIC 9(6).                                    
006300*                                 PACKNINGSDATUM SENASTE KOLLI            
006400     03 MOD-ADGMT-LAND       PIC X(27).                                   
006500*                                 DEL AV GODSMOTTAGARADRESS               
006600     03 MOD-BEFRAKT          PIC X(20).                                   
006700*                                 FRAKT TEXT                              
006800     03 MOD-VAGNNR           PIC X(8).                                    
006900     03 MOD-IDUSER           PIC X(8).                                    
007000*                                 ANVÄNDARENS SÄKERHETS ID                
007100     03 MOD-BETELNR          PIC X(10).                                   
007200*                                 TELEFONNUMMER                           
007300     03 MOD-VLORDNTO         PIC Z(3)9.9(3).                              
007400*                                 ORDERVOLYM NETTO (M3)                   
007500     03 MOD-VKORDNTO         PIC Z(5)9.9.                                 
007600*                                 ORDERVIKT NETTO (KG)                    
007700     03 MOD-KVORDRAD         PIC Z(4)9.                                   
007800*                                 ANTAL ORDERRADER                        
007900     03 MOD-LAGERAVBOK       PIC X(12).                                   
008000     03 MOD-FLLSBOK          PIC X.                                       
008100*                                 LAGERAVBOKNING                          
008200     03 MOD-BEGMRK-1         PIC X(30).                                   
008300     03 MOD-BEGMRK-2         PIC X(30).                                   
008400     03 MOD-BELAGINS-GRP.                                                 
008500*                                 LAGERINSTRUKTIONER                      
008600        05 MOD-BELAGINS-DEL1 PIC X(60).                                   
008700*                                 DEL AV LAGERINSTRUKTION                 
008800        05 MOD-BELAGINS-DEL2 PIC X(60).                                   
008900*                                 DEL AV LAGERINSTRUKTION                 
009000     03 MOD-TEMFSINF         PIC X(55).                                   
009100*                                 INFORMATIONSMEDDELANDE                  
009200*** END OF VILMAII-COPY LENGTH= 608 BYTES                                 
