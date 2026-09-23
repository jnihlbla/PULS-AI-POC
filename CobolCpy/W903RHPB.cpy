000100 01  RHPB-W903RHP.                                                        
000200*                                 NYUPPL ORDER VDI                        
000300*                                 POSTTYP = RHP                           
000400     03 RHPB-KVRADER         PIC 9(5).                                    
000500*                                 ANTAL RADER                             
000600     03 RHPB-HUVUD.                                                       
000700*                                 ORDERHUVUD VDI                          
000800        05 RHPB-KDORDKL      PIC 9.                                       
000900*                                 ORDERKLASS                              
001000        05 RHPB-KDFRAKT      PIC X(2).                                    
001100*                                 FRAKTSÄTT DC TILL KUND                  
001200        05 RHPB-FLRESTN      PIC X.                                       
001300*                                 RESTNOTERING ?                          
001400        05 RHPB-BEKUNDRF     PIC X(10).                                   
001500*                                 KUNDENS REFERENS                        
001600        05 RHPB-BEVARREF     PIC X(10).                                   
001700*                                 VÅR REFERENS                            
001800        05 RHPB-KDTPOTYP     PIC X.                                       
001900*                                 TYP AV TIDPLANERAD ORDER                
002000        05 RHPB-IDKAMPRF     PIC X(7).                                    
002100*                                 KAMPANJREFERENS                         
002200     03 RHPB-RADER           OCCURS 92 TIMES.                             
002300*                                 ORDERRADER VDI                          
002400        05 RHPB-IDARTNR      PIC 9(9).                                    
002500*                                 ARTIKELNUMMER                           
002600        05 RHPB-KVBEART      PIC 9(6).                                    
002700*                                 BESTÄLLT ANTAL STYCKEN                  
002800        05 RHPB-FLSLATT      PIC X.                                       
002900*                                 FLAGGA SOM ANGER OM KVSLATT SKA         
003000*                                 LL BERÄKNAS ELLER EJ                    
003100*                                 OM FLRESTN = J OCH FLSLATT = J,         
003200*                                  DÅ BERÄKNAS KVSLATT                    
003300        05 RHPB-KDKVBRYT     PIC X.                                       
003400*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
003500        05 RHPB-BERADREF     PIC X(10).                                   
003600*                                 KUNDENS RADREFERENS                     
003700        05 RHPB-KDDSP        PIC X.                                       
003800*                                 PÅVERKAN PÅ DSP                         
003900        05 RHPB-IDBIL.                                                    
004000*                                 BILIDENTITET                            
004100           07 RHPB-IDBILTYP  PIC X(3).                                    
004200*                                 BILTYP                                  
004300           07 RHPB-TIAAAA    PIC X(4).                                    
004400*                                 ÅRTAL (ÅÅÅÅ)                            
004500           07 RHPB-IDCHASSI-PIE                                           
004600                             PIC X(6).                                    
004700*                                 CHASSINUMMER PIE                        
004800        05 RHPB-PRARTNTO-LOC PIC 9(7)V9(2).                               
004900*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
005000        05 RHPB-PRARTBTO-LOC PIC 9(7)V9(2).                               
005100*                                 PRIS I LOKAL VALUTA                     
005200        05 RHPB-KDVALISO     PIC X(3).                                    
005300*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
005400        05 RHPB-KDVAT        PIC X(2).                                    
005500*                                 MOMSKOD                                 
005600        05 RHPB-RERAB        PIC 9(2)V9(1).                               
005700*                                 RABATTSATS (PROCENT)                    
005800        05 RHPB-KDRAB        PIC X(5).                                    
005900*                                 RABATTKOD                               
006000        05 RHPB-BEART-VIPS   PIC X(25).                                   
006100*                                 VIPS ARTIKELBENÄMNING                   
006200*                                 PÅ DEALERNS SPRÅK                       
006300*** END OF VILMAII-COPY LENGTH= 8961 BYTES                                
