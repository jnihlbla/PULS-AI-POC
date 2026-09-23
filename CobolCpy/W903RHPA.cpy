000100 01  RHPA-W903RHP.                                                        
000200*                                 NYUPPL ORDER VDI                        
000300*                                 POSTTYP = RHP                           
000400     03 RHPA-KVRADER         PIC 9(5).                                    
000500*                                 ANTAL RADER                             
000600     03 RHPA-HUVUD.                                                       
000700*                                 ORDERHUVUD VDI                          
000800        05 RHPA-KDORDKL      PIC 9.                                       
000900*                                 ORDERKLASS                              
001000        05 RHPA-KDFRAKT      PIC X(2).                                    
001100*                                 FRAKTSÄTT DC TILL KUND                  
001200        05 RHPA-FLRESTN      PIC X.                                       
001300*                                 RESTNOTERING ?                          
001400        05 RHPA-BEKUNDRF     PIC X(10).                                   
001500*                                 KUNDENS REFERENS                        
001600        05 RHPA-BEVARREF     PIC X(10).                                   
001700*                                 VÅR REFERENS                            
001800        05 RHPA-KDTPOTYP     PIC X.                                       
001900*                                 TYP AV TIDPLANERAD ORDER                
002000        05 RHPA-IDKAMPRF     PIC X(7).                                    
002100*                                 KAMPANJREFERENS                         
002200     03 RHPA-RADER           OCCURS 92 TIMES.                             
002300*                                 ORDERRADER VDI                          
002400        05 RHPA-IDARTNR      PIC 9(9).                                    
002500*                                 ARTIKELNUMMER                           
002600        05 RHPA-KVBEART      PIC 9(6).                                    
002700*                                 BESTÄLLT ANTAL STYCKEN                  
002800        05 RHPA-FLSLATT      PIC X.                                       
002900*                                 FLAGGA SOM ANGER OM KVSLATT SKA         
003000*                                 LL BERÄKNAS ELLER EJ                    
003100*                                 OM FLRESTN = J OCH FLSLATT = J,         
003200*                                  DÅ BERÄKNAS KVSLATT                    
003300        05 RHPA-KDKVBRYT     PIC X.                                       
003400*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
003500        05 RHPA-BERADREF     PIC X(10).                                   
003600*                                 KUNDENS RADREFERENS                     
003700        05 RHPA-KDDSP        PIC X.                                       
003800*                                 PÅVERKAN PÅ DSP                         
003900        05 RHPA-IDBIL.                                                    
004000*                                 BILIDENTITET                            
004100           07 RHPA-IDBILTYP  PIC X(3).                                    
004200*                                 BILTYP                                  
004300           07 RHPA-TIAAAA    PIC X(4).                                    
004400*                                 ÅRTAL (ÅÅÅÅ)                            
004500           07 RHPA-IDCHASSI-PIE                                           
004600                             PIC X(6).                                    
004700*                                 CHASSINUMMER PIE                        
004800*** END OF VILMAII-COPY LENGTH= 3809 BYTES                                
