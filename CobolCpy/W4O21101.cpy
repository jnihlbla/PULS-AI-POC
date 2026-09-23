000100 01  MOD-W4O21101.                                                        
000200*                                 MOD-COPYTEXT FÖR W4O21101               
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
002700     03 MOD-FLOH2-ATTR       PIC X(2).                                    
002800*                                 MFS ATTRIBUTFÄLT                        
002900     03 MOD-FLOH2            PIC X.                                       
003000*                                 ORDERHUVUD 2                            
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
004100     03 MOD-KDFAKTYP-ATTR    PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300     03 MOD-KDFAKTYP         PIC X.                                       
004400*                                 FAKTURATYP                              
004500     03 MOD-FLAUTFAK-ATTR    PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700     03 MOD-FLAUTFAK         PIC X.                                       
004800*                                 AUTOMATFAKTURERING ?                    
004900     03 MOD-FLRESTN-ATTR     PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100     03 MOD-FLRESTN          PIC X.                                       
005200*                                 RESTNOTERING ?                          
005300     03 MOD-KDTPOTYP-ATTR    PIC X(2).                                    
005400*                                 MFS ATTRIBUTFÄLT                        
005500     03 MOD-KDTPOTYP         PIC 9.                                       
005600*                                 TYP AV TIDPLANERAD ORDER                
005700     03 MOD-TITPO-ATTR       PIC X(2).                                    
005800*                                 MFS ATTRIBUTFÄLT                        
005900     03 MOD-TITPO            PIC 9(6).                                    
006000*                                 PLANERAD ORDERDATUM                     
006100     03 MOD-IDKAMPRF-ATTR    PIC X(2).                                    
006200*                                 MFS ATTRIBUTFÄLT                        
006300     03 MOD-IDKAMPRF         PIC Z(6)9.                                   
006400*                                 KAMPANJREFERENS                         
006500     03 MOD-BELAGINS-GRP.                                                 
006600*                                 LAGERINSTRUKTIONER                      
006700        05 MOD-BELAGINS-DEL1 PIC X(60).                                   
006800*                                 DEL AV LAGERINSTRUKTION                 
006900        05 MOD-BELAGINS-DEL2 PIC X(60).                                   
007000*                                 DEL AV LAGERINSTRUKTION                 
007100     03 MOD-BEGMT.                                                        
007200*                                 GODSMOTTAGARNAMN                        
007300        05 MOD-BEGMT-RAD1    PIC X(35).                                   
007400*                                 GODSMOTTAGARNAMN RAD 1                  
007500        05 MOD-BEGMT-RAD2    PIC X(35).                                   
007600*                                 GODSMOTTAGARNAMN RAD 2                  
007700     03 MOD-ADGMT.                                                        
007800*                                 GODSMOTTAGARADRESS                      
007900        05 MOD-ADGMT-GATA    PIC X(35).                                   
008000*                                 GODSMOTTAGARADRESS GATA                 
008100        05 MOD-ADGMT-PADR    PIC X(35).                                   
008200*                                 GODSMOTTAGARADRESS POSTADRESS           
008300        05 MOD-ADPOST-PNRORT REDEFINES MOD-ADGMT-PADR.                    
008400*                                 POSTNUMMER + ORT                        
008500           07 MOD-ADPOSTNR   PIC X(10).                                   
008600*                                 POSTNUMMER I ADRESS                     
008700           07 MOD-ADCITY     PIC X(25).                                   
008800*                                 BENÄMNING PÅ STAD                       
008900        05 MOD-ADPOST-ORTPNR REDEFINES MOD-ADGMT-PADR.                    
009000*                                 ORT + POSTNUMMER                        
009100           07 MOD-ADCITY     PIC X(25).                                   
009200*                                 BENÄMNING PÅ STAD                       
009300           07 MOD-ADPOSTNR   PIC X(10).                                   
009400*                                 POSTNUMMER I ADRESS                     
009500        05 MOD-ADGMT-LAND    PIC X(35).                                   
009600*                                 GODSMOTTAGARADRESS LAND                 
009700     03 MOD-BEGMRK.                                                       
009800*                                 GODSMÄRKE                               
009900        05 MOD-BEGMRK-RAD1   PIC X(30).                                   
010000*                                 GODSMÄRKE  RAD1                         
010100        05 MOD-BEGMRK-RAD2   PIC X(30).                                   
010200*                                 GODSMÄRKE  RAD2                         
010300     03 MOD-KDROPACK-ATTR    PIC X(2).                                    
010400*                                 MFS ATTRIBUTFÄLT                        
010500     03 MOD-KDROPACK         PIC X.                                       
010600*                                 FRISLÄPPNINGSKOD RO/DO                  
010700     03 MOD-IDBIPREF-ATTR    PIC X(2).                                    
010800*                                 MFS ATTRIBUTFÄLT                        
010900     03 MOD-IDBIPREF         PIC X(7).                                    
011000*                                 BIPACKNINGSREFERENS                     
011100     03 MOD-IDFTG-ATTR       PIC X(2).                                    
011200*                                 MFS ATTRIBUTFÄLT                        
011300     03 MOD-IDFTG            PIC 9(2).                                    
011400*                                 FÖRETAGSID EKONOM REDOVISNING           
011500     03 MOD-IDKONTO-ATTR     PIC X(2).                                    
011600*                                 MFS ATTRIBUTFÄLT                        
011700     03 MOD-IDKONTO          PIC Z(9)9.                                   
011800*                                 KONTO                                   
011900     03 MOD-IDANALYS-ATTR    PIC X(2).                                    
012000*                                 MFS ATTRIBUTFÄLT                        
012100     03 MOD-IDANALYS         PIC X(12).                                   
012200*                                 ANALYSNUMMER                            
012300     03 MOD-IDKST-ATTR       PIC X(2).                                    
012400*                                 MFS ATTRIBUTFÄLT                        
012500     03 MOD-IDKST            PIC X(10).                                   
012600*                                 KOSTNADSSTÄLLE                          
012700     03 MOD-BEVARREF         PIC X(10).                                   
012800*                                 VÅR REFERENS                            
012900     03 MOD-TEMFSINF         PIC X(55).                                   
013000*                                 INFORMATIONSMEDDELANDE                  
013100*** END OF VILMAII-COPY LENGTH= 618 BYTES                                 
