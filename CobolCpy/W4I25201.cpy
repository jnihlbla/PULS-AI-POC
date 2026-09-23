000100 01  MID-W4I25201.                                                        
000200*                                 MID-COPYTEXT FÖR W4I25201               
000300*                                 ORDERRADER                              
000400     03 MID-IDSYSTEM         PIC X(4).                                    
000500*                                 VOLVO VCCS SYSTEMNUMMER                 
000600     03 MID-IDDISTR          PIC X(4).                                    
000700*                                 DISTRIKTNUMMER                          
000800     03 MID-IDKUNDNR         PIC X(6).                                    
000900*                                 KUNDNUMMER                              
001000     03 MID-IDORDNR          PIC X(7).                                    
001100*                                 ORDERNUMMER                             
001200     03 MID-BEVOLREF         PIC X(10).                                   
001300*                                 VOLVO REFERENS                          
001400     03 MID-IDKLIENT         PIC X(10).                                   
001500*                                 VADIS KLIENT                            
001600     03 MID-IDARBREF         PIC X(10).                                   
001700*                                 ARBETSORDER VADIS                       
001800     03 MID-IDVIN            PIC X(17).                                   
001900*                                 VIN ID FORDON                           
002000     03 MID-IDKUNDRF-RO      PIC X(10).                                   
002100*                                 KUND REF PÅ RO                          
002200     03 MID-KDORDURS         PIC X.                                       
002300*                                 OREDR URSPRUNG                          
002400     03 MID-FLSLUT           PIC X.                                       
002500*                                 AVSLUTNINGSFLAGGA                       
002600     03 MID-RADER            OCCURS 5 TIMES.                              
002700        05 MID-IDARTNR       PIC X(9).                                    
002800*                                 ARTIKELNUMMER                           
002900        05 MID-REKSIFFR      PIC X.                                       
003000*                                 KONTROLLSIFFRA                          
003100        05 MID-KVBEART       PIC X(6).                                    
003200*                                 BESTÄLLT ANTAL STYCKEN                  
003300        05 MID-PRARTNTO      PIC X(10).                                   
003400*                                 ARTIKELPRIS NETTO                       
003500        05 MID-TITPO         PIC X(6).                                    
003600*                                 PLANERAD ORDERDATUM                     
003700        05 MID-FLRESTN       PIC X.                                       
003800*                                 RESTNOTERING ?                          
003900        05 MID-KDKVBRYT      PIC X.                                       
004000*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
004100        05 MID-FLINVEST      PIC X.                                       
004200*                                 BYTES INVENTERINGSFLAGGA                
004300        05 MID-KDVRINFO      PIC X.                                       
004400*                                 PÅVERKAN I VR/DSP SYSTEM                
004500        05 MID-IDKONTO       PIC X(10).                                   
004600*                                 KONTO                                   
004700        05 MID-IDKST         PIC X(10).                                   
004800*                                 KOSTNADSSTÄLLE                          
004900        05 MID-BERADREF      PIC X(10).                                   
005000*                                 KUNDENS RADREFERENS                     
005100        05 MID-KDDSP         PIC X.                                       
005200*                                 PÅVERKAN PÅ DSP                         
005300        05 MID-FLSLATT       PIC X.                                       
005400*                                 FLAGGA SOM ANGER OM KVSLATT SKA         
005500*                                 LL BERÄKNAS ELLER EJ                    
005600*                                 OM FLRESTN = J OCH FLSLATT = J,         
005700*                                  DÅ BERÄKNAS KVSLATT                    
005800        05 MID-FLDIRLEV      PIC X.                                       
005900*                                 DIREKTLEVERANS ?                        
006000        05 MID-IDBIL.                                                     
006100*                                 BILIDENTITET                            
006200           07 MID-IDBILTYP   PIC X(3).                                    
006300*                                 BILTYP                                  
006400           07 MID-TIAAAA     PIC X(4).                                    
006500*                                 ÅRTAL (ÅÅÅÅ)                            
006600           07 MID-IDCHASSI-PIE                                            
006700                             PIC X(6).                                    
006800*                                 CHASSINUMMER PIE                        
006900        05 MID-PRARTNTO-LOC  PIC X(10).                                   
007000*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
007100        05 MID-PRARTBTO-LOC  PIC X(10).                                   
007200*                                 PRIS I LOKAL VALUTA                     
007300        05 MID-KDVALISO      PIC X(3).                                    
007400*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
007500        05 MID-KDVAT         PIC X(2).                                    
007600*                                 MOMSKOD                                 
007700        05 MID-RERAB         PIC 9(2)V9(1).                               
007800*                                 RABATTSATS (PROCENT)                    
007900        05 MID-KDRAB         PIC X(5).                                    
008000*                                 RABATTKOD                               
008100        05 MID-BEART-VIPS    PIC X(25).                                   
008200*                                 VIPS ARTIKELBENÄMNING                   
008300*                                 PÅ DEALERNS SPRÅK                       
008400        05 MID-ADLAGOMR-CD   PIC 9(2).                                    
008500*                                 LAGEROMRÅDE CROSS DOCKING LAGER         
008600        05 MID-ADGANG-CD     PIC 9(2).                                    
008700*                                 GÅNG                                    
008800        05 MID-ADPLATS-CD    PIC 9(5).                                    
008900*                                 LAGERPLATSNUMMER                        
009000        05 MID-IDKUNDRF-WIP  PIC X(10).                                   
009100*                                 REPARATIONS ORDERNR, LDC KUND           
009200*** END OF VILMAII-COPY LENGTH= 875 BYTES                                 
