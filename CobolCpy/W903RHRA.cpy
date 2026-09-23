000100 01  RHRA-W903RHR.                                                        
000200*                                 NYUPPL ORDER VDI                        
000300*                                 MED ADDRESS TILLÄGG                     
000400*                                 POSTTYP = RHR                           
000500     03 RHRA-KVRADER         PIC 9(5).                                    
000600*                                 ANTAL RADER                             
000700     03 RHRA-HUVUD.                                                       
000800*                                 ORDERHUVUD VDI                          
000900        05 RHRA-KDORDKL      PIC 9.                                       
001000*                                 ORDERKLASS                              
001100        05 RHRA-KDFRAKT      PIC X(2).                                    
001200*                                 FRAKTSÄTT DC TILL KUND                  
001300        05 RHRA-FLRESTN      PIC X.                                       
001400*                                 RESTNOTERING ?                          
001500        05 RHRA-BEKUNDRF     PIC X(10).                                   
001600*                                 KUNDENS REFERENS                        
001700        05 RHRA-BEVARREF     PIC X(10).                                   
001800*                                 VÅR REFERENS                            
001900        05 RHRA-KDTPOTYP     PIC X.                                       
002000*                                 TYP AV TIDPLANERAD ORDER                
002100        05 RHRA-IDKAMPRF     PIC X(7).                                    
002200*                                 KAMPANJREFERENS                         
002300     03 RHRA-BEGMT.                                                       
002400*                                 GODSMOTTAGARNAMN                        
002500        05 RHRA-BEGMT-RAD1   PIC X(35).                                   
002600*                                 GODSMOTTAGARNAMN RAD 1                  
002700        05 RHRA-BEGMT-RAD2   PIC X(35).                                   
002800*                                 GODSMOTTAGARNAMN RAD 2                  
002900     03 RHRA-ADGMT-GATA      PIC X(35).                                   
003000*                                 GODSMOTTAGARADRESS GATA                 
003100     03 RHRA-ADGMT-PADR      PIC X(35).                                   
003200*                                 GODSMOTTAGARADRESS POSTADRESS           
003300     03 RHRA-BELAGINS-DEL    PIC X(60).                                   
003400*                                 DEL AV LAGERINSTRUKTION                 
003500     03 RHRA-RADER           OCCURS 92 TIMES.                             
003600*                                 ORDERRADER VDI                          
003700        05 RHRA-IDARTNR      PIC 9(9).                                    
003800*                                 ARTIKELNUMMER                           
003900        05 RHRA-KVBEART      PIC 9(6).                                    
004000*                                 BESTÄLLT ANTAL STYCKEN                  
004100        05 RHRA-FLSLATT      PIC X.                                       
004200*                                 FLAGGA SOM ANGER OM KVSLATT SKA         
004300*                                 LL BERÄKNAS ELLER EJ                    
004400*                                 OM FLRESTN = J OCH FLSLATT = J,         
004500*                                  DÅ BERÄKNAS KVSLATT                    
004600        05 RHRA-KDKVBRYT     PIC X.                                       
004700*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
004800        05 RHRA-BERADREF     PIC X(10).                                   
004900*                                 KUNDENS RADREFERENS                     
005000        05 RHRA-KDDSP        PIC X.                                       
005100*                                 PÅVERKAN PÅ DSP                         
005200        05 RHRA-IDBIL.                                                    
005300*                                 BILIDENTITET                            
005400           07 RHRA-IDBILTYP  PIC X(3).                                    
005500*                                 BILTYP                                  
005600           07 RHRA-TIAAAA    PIC X(4).                                    
005700*                                 ÅRTAL (ÅÅÅÅ)                            
005800           07 RHRA-IDCHASSI-PIE                                           
005900                             PIC X(6).                                    
006000*                                 CHASSINUMMER PIE                        
006100*** END OF VILMAII-COPY LENGTH= 4009 BYTES                                
