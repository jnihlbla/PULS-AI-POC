000100 01  RHKB-W903RHK.                                                        
000200*                                 TILLÄGG ORDERRADER VDI                  
000300*                                 POSTTYP = RHK                           
000400     03 RHKB-KVRADER         PIC 9(5).                                    
000500*                                 ANTAL RADER                             
000600     03 RHKB-KDORDKL         PIC 9.                                       
000700*                                 ORDERKLASS                              
000800     03 RHKB-BEKUNDRF        PIC X(10).                                   
000900*                                 KUNDENS REFERENS                        
001000     03 RHKB-RADER           OCCURS 100 TIMES.                            
001100*                                 ORDERRADER VDI                          
001200        05 RHKB-IDARTNR      PIC 9(9).                                    
001300*                                 ARTIKELNUMMER                           
001400        05 RHKB-KVBEART      PIC 9(6).                                    
001500*                                 BESTÄLLT ANTAL STYCKEN                  
001600        05 RHKB-FLSLATT      PIC X.                                       
001700*                                 FLAGGA SOM ANGER OM KVSLATT SKA         
001800*                                 LL BERÄKNAS ELLER EJ                    
001900*                                 OM FLRESTN = J OCH FLSLATT = J,         
002000*                                  DÅ BERÄKNAS KVSLATT                    
002100        05 RHKB-KDKVBRYT     PIC X.                                       
002200*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
002300        05 RHKB-BERADREF     PIC X(10).                                   
002400*                                 KUNDENS RADREFERENS                     
002500        05 RHKB-KDDSP        PIC X.                                       
002600*                                 PÅVERKAN PÅ DSP                         
002700        05 RHKB-PRARTNTO-LOC PIC 9(7)V9(2).                               
002800*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
002900        05 RHKB-PRARTBTO-LOC PIC 9(7)V9(2).                               
003000*                                 PRIS I LOKAL VALUTA                     
003100        05 RHKB-KDVALISO     PIC X(3).                                    
003200*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003300        05 RHKB-KDVAT        PIC X(2).                                    
003400*                                 MOMSKOD                                 
003500        05 RHKB-RERAB        PIC 9(2)V9(1).                               
003600*                                 RABATTSATS (PROCENT)                    
003700        05 RHKB-KDRAB        PIC X(5).                                    
003800*                                 RABATTKOD                               
003900        05 RHKB-BEART-VIPS   PIC X(25).                                   
004000*                                 VIPS ARTIKELBENÄMNING                   
004100*                                 PÅ DEALERNS SPRÅK                       
004200*** END OF VILMAII-COPY LENGTH= 8416 BYTES                                
