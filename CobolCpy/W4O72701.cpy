000100 01  MOD-W4O72701.                                                        
000200*                                 MOD-COPYTEXT FÖR W4072700               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDFAKT-IN        PIC X(7).                                    
000800*                                 FAKTURANUMMER                           
000900     03 MOD-IDFAKT-UT        PIC X(7).                                    
001000*                                 FAKTURANUMMER                           
001100     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
001200*                                 KUNDNUMMER                              
001300     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001400*                                 KUNDNUMMER                              
001500     03 MOD-IDORDNR-IN       PIC X(5).                                    
001600*                                 ORDERNUMMER UTGÅR PD90                  
001700     03 MOD-IDORDNR-UT       PIC X(5).                                    
001800*                                 ORDERNUMMER UTGÅR PD90                  
001900     03 MOD-IDKOLLI-IN       PIC X(5).                                    
002000*                                 KOLLINUMMER                             
002100     03 MOD-IDKOLLI-UT       PIC X(5).                                    
002200*                                 KOLLINUMMER                             
002300     03 MOD-IDPRODNR-IN      PIC X(7).                                    
002400*                                 PRODUKTIONSNUMMER                       
002500     03 MOD-IDPRODNR-UT      PIC X(7).                                    
002600*                                 PRODUKTIONSNUMMER                       
002700     03 MOD-IDARTNR-IN       PIC X(9).                                    
002800*                                 ARTIKELNUMMER                           
002900     03 MOD-IDARTNR-UT       PIC X(9).                                    
003000*                                 ARTIKELNUMMER                           
003100     03 MOD-IDPRODNR-ENTER   PIC 9(7).                                    
003200*                                 PRODUKTIONSNUMMER                       
003300     03 MOD-IDKOLLI-ENTER    PIC 9(5).                                    
003400*                                 KOLLINUMMER                             
003500     03 MOD-IDARTNR-ENTER    PIC 9(9).                                    
003600*                                 ARTIKELNUMMER                           
003700     03 MOD-IDLOPNR-ENTER    PIC 9(3).                                    
003800*                                 LÖPNUMMER                               
003900     03 MOD-IDPRODNR-NEXT    PIC 9(7).                                    
004000*                                 PRODUKTIONSNUMMER                       
004100     03 MOD-IDKOLLI-NEXT     PIC 9(5).                                    
004200*                                 KOLLINUMMER                             
004300     03 MOD-IDARTNR-NEXT     PIC 9(9).                                    
004400*                                 ARTIKELNUMMER                           
004500     03 MOD-IDLOPNR-NEXT     PIC 9(3).                                    
004600*                                 LÖPNUMMER                               
004700     03 MOD-RADER            OCCURS 14 TIMES.                             
004800*                                 RADINFORMATION                          
004900        05 MOD-IDDISTR       PIC Z(3)9.                                   
005000*                                 DISTRIKTNUMMER                          
005100        05 MOD-IDKUNDNR      PIC Z(5)9.                                   
005200*                                 KUNDNUMMER                              
005300        05 MOD-IDORDNR7      PIC Z(6)9.                                   
005400*                                 ORDERNUMMER                             
005500        05 MOD-IDPRODNR      PIC Z(6)9.                                   
005600*                                 PRODUKTIONSNUMMER                       
005700        05 MOD-IDDC          PIC X(2).                                    
005800*                                 IDENTIFIERARE LAGER                     
005900        05 MOD-IDKOLLI       PIC Z(4)9.                                   
006000*                                 KOLLINUMMER                             
006100        05 MOD-IDARTNR       PIC Z(8)9.                                   
006200*                                 ARTIKELNUMMER                           
006300        05 MOD-KVBEART       PIC Z(5)9.                                   
006400*                                 BESTÄLLT ANTAL STYCKEN                  
006500        05 MOD-KVAVBART      PIC Z(5)9.                                   
006600*                                 AVBOKAT ANTAL ARTIKLAR                  
006700        05 MOD-KVLEVART      PIC Z(5)9.                                   
006800*                                 LEVERERAT ANTAL STYCK                   
006900        05 MOD-IDLEVNR       PIC X(5).                                    
007000*                                 LEVERANTÖRNUMMER                        
007100     03 MOD-TEMFSINF         PIC X(55).                                   
007200*                                 INFORMATIONSMEDDELANDE                  
007300*** END OF VILMAII-COPY LENGTH= 1107 BYTES                                
