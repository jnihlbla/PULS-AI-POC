000100 01  MID-W4I21101.                                                        
000200*                                 MID-COPYTEXT FÖR W4I21101               
000300     03 MID-IDDISTR          PIC X(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 MID-IDKUNDNR         PIC X(6).                                    
000600*                                 KUNDNUMMER                              
000700     03 MID-IDORDNR          PIC X(5).                                    
000800*                                 ORDERNUMMER                             
000900     03 MID-KDORDKL          PIC X.                                       
001000*                                 ORDERKLASS                              
001100     03 MID-KDFRAKT          PIC X(2).                                    
001200*                                 FRAKTSÄTT DC TILL KUND                  
001300     03 MID-FLOH2            PIC X.                                       
001400*                                 ORDERHUVUD 2                            
001500     03 MID-TIRFS-DAT        PIC X(6).                                    
001600     03 MID-TIRFS-TID        PIC X(4).                                    
001700     03 MID-BEKUNDRF         PIC X(15).                                   
001800*                                 KUNDENS REFERENS                        
001900     03 MID-KDFAKTYP         PIC X.                                       
002000*                                 FAKTURATYP                              
002100     03 MID-FLAUTFAK         PIC X.                                       
002200*                                 AUTOMATFAKTURERING ?                    
002300     03 MID-FLRESTN          PIC X.                                       
002400*                                 RESTNOTERING ?                          
002500     03 MID-KDTPOTYP         PIC X.                                       
002600*                                 TYP AV TIDPLANERAD ORDER                
002700     03 MID-TITPO            PIC X(6).                                    
002800*                                 PLANERAD ORDERDATUM                     
002900     03 MID-IDKAMPRF         PIC X(7).                                    
003000*                                 KAMPANJREFERENS                         
003100     03 MID-BELAGINS-GRP.                                                 
003200*                                 LAGERINSTRUKTIONER                      
003300        05 MID-BELAGINS-DEL1 PIC X(60).                                   
003400*                                 DEL AV LAGERINSTRUKTION                 
003500        05 MID-BELAGINS-DEL2 PIC X(60).                                   
003600*                                 DEL AV LAGERINSTRUKTION                 
003700     03 MID-BEGMT.                                                        
003800*                                 GODSMOTTAGARNAMN                        
003900        05 MID-BEGMT-RAD1    PIC X(35).                                   
004000*                                 GODSMOTTAGARNAMN RAD 1                  
004100        05 MID-BEGMT-RAD2    PIC X(35).                                   
004200*                                 GODSMOTTAGARNAMN RAD 2                  
004300     03 MID-ADGMT.                                                        
004400*                                 GODSMOTTAGARADRESS                      
004500        05 MID-ADGMT-GATA    PIC X(35).                                   
004600*                                 GODSMOTTAGARADRESS GATA                 
004700        05 MID-ADGMT-PADR    PIC X(35).                                   
004800*                                 GODSMOTTAGARADRESS POSTADRESS           
004900        05 MID-ADPOST-PNRORT REDEFINES MID-ADGMT-PADR.                    
005000*                                 POSTNUMMER + ORT                        
005100           07 MID-ADPOSTNR   PIC X(10).                                   
005200*                                 POSTNUMMER I ADRESS                     
005300           07 MID-ADCITY     PIC X(25).                                   
005400*                                 BENÄMNING PÅ STAD                       
005500        05 MID-ADPOST-ORTPNR REDEFINES MID-ADGMT-PADR.                    
005600*                                 ORT + POSTNUMMER                        
005700           07 MID-ADCITY     PIC X(25).                                   
005800*                                 BENÄMNING PÅ STAD                       
005900           07 MID-ADPOSTNR   PIC X(10).                                   
006000*                                 POSTNUMMER I ADRESS                     
006100        05 MID-ADGMT-LAND    PIC X(35).                                   
006200*                                 GODSMOTTAGARADRESS LAND                 
006300     03 MID-BEGMRK.                                                       
006400*                                 GODSMÄRKE                               
006500        05 MID-BEGMRK-RAD1   PIC X(30).                                   
006600*                                 GODSMÄRKE  RAD1                         
006700        05 MID-BEGMRK-RAD2   PIC X(30).                                   
006800*                                 GODSMÄRKE  RAD2                         
006900     03 MID-KDROPACK         PIC X.                                       
007000*                                 FRISLÄPPNINGSKOD RO/DO                  
007100     03 MID-IDBIPREF         PIC X(7).                                    
007200*                                 BIPACKNINGSREFERENS                     
007300     03 MID-IDFTG            PIC X(2).                                    
007400*                                 FÖRETAGSID EKONOM REDOVISNING           
007500     03 MID-IDKONTO          PIC X(10).                                   
007600*                                 KONTO                                   
007700     03 MID-IDANALYS         PIC X(12).                                   
007800*                                 ANALYSNUMMER                            
007900     03 MID-IDKST            PIC X(10).                                   
008000*                                 KOSTNADSSTÄLLE                          
008100     03 MID-BEVARREF         PIC X(10).                                   
008200*                                 VÅR REFERENS                            
008300*** END OF VILMAII-COPY LENGTH= 468 BYTES                                 
