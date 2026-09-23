000100 01  MID-W4I24101.                                                        
000200*                                 MID-COPYTEXT FOR W4I24101               
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
001300     03 MID-FLVORFK          PIC X.                                       
001400*                                 VOR-FRAKTKOD FRÅN KLASS 1               
001500     03 MID-TIRFS-DAT        PIC X(6).                                    
001600     03 MID-TIRFS-TID        PIC X(4).                                    
001700     03 MID-BEKUNDRF         PIC X(15).                                   
001800*                                 KUNDENS REFERENS                        
001900     03 MID-FLRESTN          PIC X.                                       
002000*                                 RESTNOTERING ?                          
002100     03 MID-KDTPOTYP         PIC X.                                       
002200*                                 TYP AV TIDPLANERAD ORDER                
002300     03 MID-TITPO            PIC X(6).                                    
002400*                                 PLANERAD ORDERDATUM                     
002500     03 MID-IDKAMPRF         PIC X(7).                                    
002600*                                 KAMPANJREFERENS                         
002700     03 MID-BELAGINS-GRP.                                                 
002800*                                 LAGERINSTRUKTIONER                      
002900        05 MID-BELAGINS-DEL1 PIC X(60).                                   
003000*                                 DEL AV LAGERINSTRUKTION                 
003100        05 MID-BELAGINS-DEL2 PIC X(60).                                   
003200*                                 DEL AV LAGERINSTRUKTION                 
003300     03 MID-BEGMT.                                                        
003400*                                 GODSMOTTAGARNAMN                        
003500        05 MID-BEGMT-RAD1    PIC X(35).                                   
003600*                                 GODSMOTTAGARNAMN RAD 1                  
003700        05 MID-BEGMT-RAD2    PIC X(35).                                   
003800*                                 GODSMOTTAGARNAMN RAD 2                  
003900     03 MID-ADGMT.                                                        
004000*                                 GODSMOTTAGARADRESS                      
004100        05 MID-ADGMT-GATA    PIC X(35).                                   
004200*                                 GODSMOTTAGARADRESS GATA                 
004300        05 MID-ADGMT-PADR    PIC X(35).                                   
004400*                                 GODSMOTTAGARADRESS POSTADRESS           
004500        05 MID-ADPOST-PNRORT REDEFINES MID-ADGMT-PADR.                    
004600*                                 POSTNUMMER + ORT                        
004700           07 MID-ADPOSTNR   PIC X(10).                                   
004800*                                 POSTNUMMER I ADRESS                     
004900           07 MID-ADCITY     PIC X(25).                                   
005000*                                 BENÄMNING PÅ STAD                       
005100        05 MID-ADPOST-ORTPNR REDEFINES MID-ADGMT-PADR.                    
005200*                                 ORT + POSTNUMMER                        
005300           07 MID-ADCITY     PIC X(25).                                   
005400*                                 BENÄMNING PÅ STAD                       
005500           07 MID-ADPOSTNR   PIC X(10).                                   
005600*                                 POSTNUMMER I ADRESS                     
005700        05 MID-ADGMT-LAND    PIC X(35).                                   
005800*                                 GODSMOTTAGARADRESS LAND                 
005900     03 MID-BEGMRK.                                                       
006000*                                 GODSMÄRKE                               
006100        05 MID-BEGMRK-RAD1   PIC X(30).                                   
006200*                                 GODSMÄRKE  RAD1                         
006300        05 MID-BEGMRK-RAD2   PIC X(30).                                   
006400*                                 GODSMÄRKE  RAD2                         
006500     03 MID-KDROPACK         PIC X.                                       
006600*                                 FRISLÄPPNINGSKOD RO/DO                  
006700     03 MID-IDBIPREF         PIC X(7).                                    
006800*                                 BIPACKNINGSREFERENS                     
006900*** END OF VILMAII-COPY LENGTH= 422 BYTES                                 
