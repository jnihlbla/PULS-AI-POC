000100 01  RHRB-W903RHR.                                                        
000200*                                 NYUPPL ORDER VDI                        
000300*                                 MED ADDRESS TILLÄGG                     
000400*                                 POSTTYP = RHR                           
000500     03 RHRB-KVRADER         PIC 9(5).                                    
000600*                                 ANTAL RADER                             
000700     03 RHRB-HUVUD.                                                       
000800*                                 ORDERHUVUD VDI                          
000900        05 RHRB-KDORDKL      PIC 9.                                       
001000*                                 ORDERKLASS                              
001100        05 RHRB-KDFRAKT      PIC X(2).                                    
001200*                                 FRAKTSÄTT DC TILL KUND                  
001300        05 RHRB-FLRESTN      PIC X.                                       
001400*                                 RESTNOTERING ?                          
001500        05 RHRB-BEKUNDRF     PIC X(10).                                   
001600*                                 KUNDENS REFERENS                        
001700        05 RHRB-BEVARREF     PIC X(10).                                   
001800*                                 VÅR REFERENS                            
001900        05 RHRB-KDTPOTYP     PIC X.                                       
002000*                                 TYP AV TIDPLANERAD ORDER                
002100        05 RHRB-IDKAMPRF     PIC X(7).                                    
002200*                                 KAMPANJREFERENS                         
002300     03 RHRB-BEGMT.                                                       
002400*                                 GODSMOTTAGARNAMN                        
002500        05 RHRB-BEGMT-RAD1   PIC X(35).                                   
002600*                                 GODSMOTTAGARNAMN RAD 1                  
002700        05 RHRB-BEGMT-RAD2   PIC X(35).                                   
002800*                                 GODSMOTTAGARNAMN RAD 2                  
002900     03 RHRB-ADGMT-GATA      PIC X(35).                                   
003000*                                 GODSMOTTAGARADRESS GATA                 
003100     03 RHRB-ADGMT-PADR      PIC X(35).                                   
003200*                                 GODSMOTTAGARADRESS POSTADRESS           
003300     03 RHRB-BELAGINS-DEL    PIC X(60).                                   
003400*                                 DEL AV LAGERINSTRUKTION                 
003500     03 RHRB-RADER           OCCURS 92 TIMES.                             
003600*                                 ORDERRADER VDI                          
003700        05 RHRB-IDARTNR      PIC 9(9).                                    
003800*                                 ARTIKELNUMMER                           
003900        05 RHRB-KVBEART      PIC 9(6).                                    
004000*                                 BESTÄLLT ANTAL STYCKEN                  
004100        05 RHRB-FLSLATT      PIC X.                                       
004200*                                 FLAGGA SOM ANGER OM KVSLATT SKA         
004300*                                 LL BERÄKNAS ELLER EJ                    
004400*                                 OM FLRESTN = J OCH FLSLATT = J,         
004500*                                  DÅ BERÄKNAS KVSLATT                    
004600        05 RHRB-KDKVBRYT     PIC X.                                       
004700*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
004800        05 RHRB-BERADREF     PIC X(10).                                   
004900*                                 KUNDENS RADREFERENS                     
005000        05 RHRB-KDDSP        PIC X.                                       
005100*                                 PÅVERKAN PÅ DSP                         
005200        05 RHRB-IDBIL.                                                    
005300*                                 BILIDENTITET                            
005400           07 RHRB-IDBILTYP  PIC X(3).                                    
005500*                                 BILTYP                                  
005600           07 RHRB-TIAAAA    PIC X(4).                                    
005700*                                 ÅRTAL (ÅÅÅÅ)                            
005800           07 RHRB-IDCHASSI-PIE                                           
005900                             PIC X(6).                                    
006000*                                 CHASSINUMMER PIE                        
006100        05 RHRB-PRARTNTO-LOC PIC 9(7)V9(2).                               
006200*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
006300        05 RHRB-PRARTBTO-LOC PIC 9(7)V9(2).                               
006400*                                 PRIS I LOKAL VALUTA                     
006500        05 RHRB-KDVALISO     PIC X(3).                                    
006600*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
006700        05 RHRB-KDVAT        PIC X(2).                                    
006800*                                 MOMSKOD                                 
006900        05 RHRB-RERAB        PIC 9(2)V9(1).                               
007000*                                 RABATTSATS (PROCENT)                    
007100        05 RHRB-KDRAB        PIC X(5).                                    
007200*                                 RABATTKOD                               
007300        05 RHRB-BEART-VIPS   PIC X(25).                                   
007400*                                 VIPS ARTIKELBENÄMNING                   
007500*                                 PÅ DEALERNS SPRÅK                       
007600*** END OF VILMAII-COPY LENGTH= 9161 BYTES                                
