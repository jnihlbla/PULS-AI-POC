000100 01  MOD-W30305O1.                                                        
000200*                                 MOD-COPYTEXT FÖR W3030500               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC Z(8)9.                                   
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDDISTR-IN       PIC Z(3)9.                                   
001000*                                 DISTRIKTNUMMER                          
001100     03 MOD-IDKUNDNR-IN      PIC Z(5)9.                                   
001200*                                 KUNDNUMMER                              
001300     03 MOD-KDORDKL-IN       PIC 9.                                       
001400*                                 ORDERKLASS                              
001500     03 MOD-KVBEART-IN       PIC Z(5)9.                                   
001600*                                 BESTÄLLT ANTAL STYCKEN                  
001700     03 MOD-IDARTNR-UT       PIC Z(8)9.                                   
001800*                                 ARTIKELNUMMER                           
001900     03 MOD-IDDISTR-UT       PIC Z(3)9.                                   
002000*                                 DISTRIKTNUMMER                          
002100     03 MOD-IDKUNDNR-UT      PIC Z(5)9.                                   
002200*                                 KUNDNUMMER                              
002300     03 MOD-KDORDKL-UT       PIC 9.                                       
002400*                                 ORDERKLASS                              
002500     03 MOD-KVBEART-UT       PIC Z(5)9.                                   
002600*                                 BESTÄLLT ANTAL STYCKEN                  
002700     03 MOD-BEART-VIPS       PIC X(25).                                   
002800*                                 VIPS ARTIKELBENÄMNING                   
002900*                                 PÅ DEALERNS SPRÅK                       
003000     03 MOD-KDVALISO         PIC X(3).                                    
003100*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003200     03 MOD-PRARTBTO-LOC     PIC Z(6)9.9(2).                              
003300*                                 PRIS I LOKAL VALUTA                     
003400     03 MOD-PRARTNTO-LOC     PIC Z(6)9.9(2).                              
003500*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
003600     03 MOD-KDRAB            PIC X(5).                                    
003700*                                 RABATTKOD                               
003800     03 MOD-PRARTSJK-MONLOC  PIC Z(6)9.9(2).                              
003900*                                 ARTIKELNS SJÄLVKOSTNAD TILL MÅN         
004000*                                 ADSKURS                                 
004100     03 MOD-REBVSPPR         PIC Z(2)9.9.                                 
004200*                                 BRUTTOVINSTPROCENT                      
004300     03 MOD-TEMFSINF         PIC X(55).                                   
004400*                                 INFORMATIONSMEDDELANDE                  
004500*** END OF VILMAII-COPY LENGTH= 219 BYTES                                 
