000100 01  MOD-W4O24101.                                                        
000200*                                 MOD-COPYTEXT FÖR W4O24101               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDISTR-ATTR     PIC X(2).                                    
000800*                                 MFS ATTRIBUTFÄLT                        
000900     03 MOD-IDDISTR          PIC Z(3)9.                                   
001000*                                 DISTRIKTNUMMER                          
001100     03 MOD-IDKUNDNR-ATTR    PIC X(2).                                    
001200*                                 MFS ATTRIBUTFÄLT                        
001300     03 MOD-IDKUNDNR         PIC Z(5)9.                                   
001400*                                 KUNDNUMMER                              
001500     03 MOD-IDORDNR-ATTR     PIC X(2).                                    
001600*                                 MFS ATTRIBUTFÄLT                        
001700     03 MOD-IDORDNR          PIC Z(4)9.                                   
001800*                                 ORDERNUMMER                             
001900     03 MOD-KDORDKL-ATTR     PIC X(2).                                    
002000*                                 MFS ATTRIBUTFÄLT                        
002100     03 MOD-KDORDKL          PIC 9.                                       
002200*                                 ORDERKLASS                              
002300     03 MOD-KDFRAKT-ATTR     PIC X(2).                                    
002400*                                 MFS ATTRIBUTFÄLT                        
002500     03 MOD-KDFRAKT          PIC Z9.                                      
002600*                                 FRAKTSÄTT DC TILL KUND                  
002700     03 MOD-FLVORFK-ATTR     PIC X(2).                                    
002800*                                 MFS ATTRIBUTFÄLT                        
002900     03 MOD-FLVORFK          PIC X.                                       
003000*                                 VOR-FRAKTKOD FRÅN KLASS 1               
003100     03 MOD-TEDDI            PIC X(11).                                   
003200*                                 TEXTFÄLT DDI                            
003300     03 MOD-TIRFS-DAT-ATTR   PIC X(2).                                    
003400*                                 MFS ATTRIBUTFÄLT                        
003500     03 MOD-TIRFS-DAT        PIC X(6).                                    
003600     03 MOD-TIRFS-TID-ATTR   PIC X(2).                                    
003700*                                 MFS ATTRIBUTFÄLT                        
003800     03 MOD-TIRFS-TID        PIC X(4).                                    
003900     03 MOD-BEKUNDRF         PIC X(15).                                   
004000*                                 KUNDENS REFERENS                        
004100     03 MOD-FLRESTN-ATTR     PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300     03 MOD-FLRESTN          PIC X.                                       
004400*                                 RESTNOTERING ?                          
004500     03 MOD-KDTPOTYP-ATTR    PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700     03 MOD-KDTPOTYP         PIC 9.                                       
004800*                                 TYP AV TIDPLANERAD ORDER                
004900     03 MOD-TITPO-ATTR       PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100     03 MOD-TITPO            PIC 9(6).                                    
005200*                                 PLANERAD ORDERDATUM                     
005300     03 MOD-IDKAMPRF-ATTR    PIC X(2).                                    
005400*                                 MFS ATTRIBUTFÄLT                        
005500     03 MOD-IDKAMPRF         PIC Z(6)9.                                   
005600*                                 KAMPANJREFERENS                         
005700     03 MOD-BELAGINS-GRP.                                                 
005800*                                 LAGERINSTRUKTIONER                      
005900        05 MOD-BELAGINS-DEL1 PIC X(60).                                   
006000*                                 DEL AV LAGERINSTRUKTION                 
006100        05 MOD-BELAGINS-DEL2 PIC X(60).                                   
006200*                                 DEL AV LAGERINSTRUKTION                 
006300     03 MOD-BEGMT.                                                        
006400*                                 GODSMOTTAGARNAMN                        
006500        05 MOD-BEGMT-RAD1    PIC X(35).                                   
006600*                                 GODSMOTTAGARNAMN RAD 1                  
006700        05 MOD-BEGMT-RAD2    PIC X(35).                                   
006800*                                 GODSMOTTAGARNAMN RAD 2                  
006900     03 MOD-ADGMT.                                                        
007000*                                 GODSMOTTAGARADRESS                      
007100        05 MOD-ADGMT-GATA    PIC X(35).                                   
007200*                                 GODSMOTTAGARADRESS GATA                 
007300        05 MOD-ADGMT-PADR    PIC X(35).                                   
007400*                                 GODSMOTTAGARADRESS POSTADRESS           
007500        05 MOD-ADPOST-PNRORT REDEFINES MOD-ADGMT-PADR.                    
007600*                                 POSTNUMMER + ORT                        
007700           07 MOD-ADPOSTNR   PIC X(10).                                   
007800*                                 POSTNUMMER I ADRESS                     
007900           07 MOD-ADCITY     PIC X(25).                                   
008000*                                 BENÄMNING PÅ STAD                       
008100        05 MOD-ADPOST-ORTPNR REDEFINES MOD-ADGMT-PADR.                    
008200*                                 ORT + POSTNUMMER                        
008300           07 MOD-ADCITY     PIC X(25).                                   
008400*                                 BENÄMNING PÅ STAD                       
008500           07 MOD-ADPOSTNR   PIC X(10).                                   
008600*                                 POSTNUMMER I ADRESS                     
008700        05 MOD-ADGMT-LAND    PIC X(35).                                   
008800*                                 GODSMOTTAGARADRESS LAND                 
008900     03 MOD-BEGMRK.                                                       
009000*                                 GODSMÄRKE                               
009100        05 MOD-BEGMRK-RAD1   PIC X(30).                                   
009200*                                 GODSMÄRKE  RAD1                         
009300        05 MOD-BEGMRK-RAD2   PIC X(30).                                   
009400*                                 GODSMÄRKE  RAD2                         
009500     03 MOD-KDROPACK-ATTR    PIC X(2).                                    
009600*                                 MFS ATTRIBUTFÄLT                        
009700     03 MOD-KDROPACK         PIC X.                                       
009800*                                 FRISLÄPPNINGSKOD RO/DO                  
009900     03 MOD-IDBIPREF-ATTR    PIC X(2).                                    
010000*                                 MFS ATTRIBUTFÄLT                        
010100     03 MOD-IDBIPREF         PIC X(7).                                    
010200*                                 BIPACKNINGSREFERENS                     
010300     03 MOD-TEMFSINF         PIC X(55).                                   
010400*                                 INFORMATIONSMEDDELANDE                  
010500*** END OF VILMAII-COPY LENGTH= 560 BYTES                                 
