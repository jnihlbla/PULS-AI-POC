000100 01  MOD-W4O20101.                                                        
000200*                                 MOD-COPYTEXT FÖR W4020100               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDISTR-IN       PIC X(2).                                    
000800*                                 MFS BEHANDLING AV INPUTFÄLT             
000900     03 MOD-IDDISTR-UT       PIC X(4).                                    
001000*                                 DISTRIKTNUMMER                          
001100     03 MOD-IDKUNDNR-IN      PIC X(2).                                    
001200*                                 MFS BEHANDLING AV INPUTFÄLT             
001300     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001400*                                 KUNDNUMMER                              
001500     03 MOD-IDORDNR7-IN      PIC X(2).                                    
001600*                                 MFS BEHANDLING AV INPUTFÄLT             
001700     03 MOD-IDORDNR7-UT      PIC X(7).                                    
001800*                                 ORDERNUMMER                             
001900     03 MOD-IDDC-IN          PIC X(2).                                    
002000*                                 IDENTIFIERARE LAGER                     
002100     03 MOD-IDDC-UT          PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300     03 MOD-TEDDI            PIC X(11).                                   
002400*                                 TEXTFÄLT DDI                            
002500     03 MOD-SUORDV           PIC Z(8)9.9(2).                              
002600*                                 SUMMA ORDERVÄRDE                        
002700     03 MOD-TEASTRIX         PIC X.                                       
002800*                                 ASTERISK                                
002900     03 MOD-KVORDRAD         PIC Z(4)9.                                   
003000*                                 ANTAL ORDERRADER                        
003100     03 MOD-OUTPUT.                                                       
003200*                                 DATADEL MOD, ÄNDRING ORDERHUVUD         
003300        05 MOD-BELAGINS-GRP.                                              
003400*                                 LAGERINSTRUKTIONER                      
003500           07 MOD-BELAGINS-DEL1                                           
003600                             PIC X(60).                                   
003700*                                 DEL AV LAGERINSTRUKTION                 
003800           07 MOD-BELAGINS-DEL2                                           
003900                             PIC X(60).                                   
004000*                                 DEL AV LAGERINSTRUKTION                 
004100        05 MOD-BEGMT.                                                     
004200*                                 GODSMOTTAGARNAMN                        
004300           07 MOD-BEGMT-RAD1 PIC X(35).                                   
004400*                                 GODSMOTTAGARNAMN RAD 1                  
004500           07 MOD-BEGMT-RAD2 PIC X(35).                                   
004600*                                 GODSMOTTAGARNAMN RAD 2                  
004700        05 MOD-ADGMT.                                                     
004800*                                 GODSMOTTAGARADRESS                      
004900           07 MOD-ADGMT-GATA PIC X(35).                                   
005000*                                 GODSMOTTAGARADRESS GATA                 
005100           07 MOD-ADGMT-PADR PIC X(35).                                   
005200*                                 GODSMOTTAGARADRESS POSTADRESS           
005300           07 MOD-ADGMT-LAND PIC X(35).                                   
005400*                                 GODSMOTTAGARADRESS LAND                 
005500        05 MOD-BEGMRK.                                                    
005600*                                 GODSMÄRKE                               
005700           07 MOD-BEGMRK-RAD1                                             
005800                             PIC X(30).                                   
005900*                                 GODSMÄRKE  RAD1                         
006000           07 MOD-BEGMRK-RAD2                                             
006100                             PIC X(30).                                   
006200*                                 GODSMÄRKE  RAD2                         
006300        05 MOD-KDFRAKT-ATTR  PIC X(2).                                    
006400*                                 MFS ATTRIBUTFÄLT                        
006500        05 MOD-KDFRAKT       PIC Z9.                                      
006600*                                 FRAKTSÄTT DC TILL KUND                  
006700        05 MOD-BEKUNDRF      PIC X(15).                                   
006800*                                 KUNDENS REFERENS                        
006900     03 MOD-TEMFSINF         PIC X(55).                                   
007000*                                 INFORMATIONSMEDDELANDE                  
007100*** END OF VILMAII-COPY LENGTH= 529 BYTES                                 
