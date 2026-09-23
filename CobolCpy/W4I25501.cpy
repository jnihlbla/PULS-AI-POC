000100 01  MID-W4I25501-CTX.                                                    
000200*                                 MID-COPYTEXT FÖR W4I25501               
000300*                                 ORDERRADER                              
000400     03 MID-IDSYSTEM         PIC X(4).                                    
000500*                                 VOLVO VCCS SYSTEMNUMMER                 
000600     03 MID-IDDISTR          PIC X(4).                                    
000700*                                 DISTRIKTNUMMER                          
000800     03 MID-IDKUNDNR         PIC X(6).                                    
000900*                                 KUNDNUMMER                              
001000     03 MID-IDORDNR          PIC X(7).                                    
001100*                                 ORDERNUMMER                             
001200     03 MID-KDORDKL          PIC 9.                                       
001300*                                 ORDERKLASS                              
001400     03 MID-BEKUNDRF-001     PIC X(10).                                   
001500*                                 KUNDENS REFERENS                        
001600     03 MID-IDMFSMED         PIC X(3).                                    
001700*                                 MFS MEDDELANDE NUMMER                   
001800     03 MID-FLSLUT           PIC X.                                       
001900*                                 AVSLUTNINGSFLAGGA                       
002000     03 MID-W4I25501-001-GRP OCCURS 8 TIMES.                              
002100        05 MID-IDARTNR       PIC X(9).                                    
002200*                                 ARTIKELNUMMER                           
002300        05 MID-KVBEART       PIC X(6).                                    
002400*                                 BESTÄLLT ANTAL STYCKEN                  
002500        05 MID-KDKVBRYT      PIC X.                                       
002600*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
002700        05 MID-BERADREF      PIC X(10).                                   
002800*                                 KUNDENS RADREFERENS                     
002900        05 MID-KDDSP         PIC X.                                       
003000*                                 PÅVERKAN PÅ DSP                         
003100        05 MID-FLSLATT       PIC X.                                       
003200*                                 FLAGGA SOM ANGER OM KVSLATT SKA         
003300*                                 LL BERÄKNAS ELLER EJ                    
003400*                                 OM FLRESTN = J OCH FLSLATT = J,         
003500*                                  DÅ BERÄKNAS KVSLATT                    
003600        05 MID-IDPRQUES      PIC X(7).                                    
003700*                                 PRISFRÅGA NR                            
003800        05 MID-PRARTNTO-LOC  PIC X(10).                                   
003900*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
004000        05 MID-PRARTNTO-LOCPREL                                           
004100                             PIC X(10).                                   
004200*                                 PREL NETTO SLUTKUNDSPRIS I              
004300*                                 LOKAL VALUTA                            
004400        05 MID-PRARTBTO-LOC  PIC X(10).                                   
004500*                                 PRIS I LOKAL VALUTA                     
004600        05 MID-KDVALISO      PIC X(3).                                    
004700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
004800        05 MID-KDVAT         PIC X(2).                                    
004900*                                 MOMSKOD                                 
005000        05 MID-RERAB         PIC 9(2)V9(1).                               
005100*                                 RABATTSATS (PROCENT)                    
005200        05 MID-KDRAB         PIC X(5).                                    
005300*                                 RABATTKOD                               
005400        05 MID-BEART-VIPS    PIC X(25).                                   
005500*                                 VIPS ARTIKELBENÄMNING                   
005600*                                 PÅ DEALERNS SPRÅK                       
005700        05 MID-IDKUNDRF-WIP  PIC X(10).                                   
005800*                                 REPARATIONS ORDERNR, LDC KUND           
005900*** END OF VILMAII-COPY LENGTH= 940 BYTES                                 
