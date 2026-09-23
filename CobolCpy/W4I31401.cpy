000100 01  MID-W4I31401.                                                        
000200*                                 MID-COPYTEXT PGM W40314                 
000300*                                 KOLLIRAPPORTERING-1                     
000400     03 MID-IDANSTNR-IN      PIC X(5).                                    
000500*                                 ANSTÄLLNINGSNUMMER                      
000600     03 MID-IDANSTNR-UT      PIC X(5).                                    
000700*                                 ANSTÄLLNINGSNUMMER                      
000800     03 MID-IDDISTR-IN       PIC X(4).                                    
000900*                                 DISTRIKTNUMMER                          
001000     03 MID-IDDISTR-UT       PIC X(4).                                    
001100*                                 DISTRIKTNUMMER                          
001200     03 MID-IDKUNDNR-IN      PIC X(6).                                    
001300*                                 KUNDNUMMER                              
001400     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001500*                                 KUNDNUMMER                              
001600     03 MID-IDORDNR-IN       PIC X(5).                                    
001700*                                 ORDERNUMMER UTGÅR PD90                  
001800     03 MID-IDORDNR-UT       PIC X(5).                                    
001900*                                 ORDERNUMMER UTGÅR PD90                  
002000     03 MID-IDKOLLI-IN       PIC X(5).                                    
002100*                                 KOLLINUMMER                             
002200     03 MID-IDKOLLI-UT       PIC X(5).                                    
002300*                                 KOLLINUMMER                             
002400     03 MID-IDPRODNR-IN      PIC X(7).                                    
002500*                                 PRODUKTIONSNUMMER                       
002600     03 MID-IDPRODNR-UT      PIC X(7).                                    
002700*                                 PRODUKTIONSNUMMER                       
002800     03 MID-IDDC-IN          PIC X(2).                                    
002900*                                 IDENTIFIERARE LAGER                     
003000     03 MID-IDDC-UT          PIC X(2).                                    
003100*                                 IDENTIFIERARE LAGER                     
003200     03 MID-IDTRANS-START    PIC X(4).                                    
003300*                                 BILDNUMMER                              
003400     03 MID-FLSISTAK         PIC X.                                       
003500*                                 SISTA KOLLI I ORDERN?                   
003600     03 MID-FLFORTSK         PIC X.                                       
003700*                                                                         
003800     03 MID-IDRADNR-FOM-S    PIC 9(4).                                    
003900*                                 RADNUMMER                               
004000     03 MID-IDRADNR-TOM-S    PIC 9(4).                                    
004100*                                 RADNUMMER                               
004200     03 MID-KVLEVART-S       PIC 9(6).                                    
004300*                                 LEVERERAT ANTAL STYCK                   
004400     03 MID-KDPRTVAL-ADRESSFL                                             
004500                             PIC X(2).                                    
004600*                                 PRINTER-VAL KOD                         
004700     03 MID-KDPRTVAL-FOLJEFL PIC X(2).                                    
004800*                                 PRINTER-VAL KOD                         
004900     03 MID-RAD              OCCURS 12 TIMES.                             
005000        05 MID-IDRADNR-FOM   PIC X(4).                                    
005100*                                 RADNUMMER                               
005200        05 MID-IDRADNR-TOM   PIC X(4).                                    
005300*                                 RADNUMMER                               
005400        05 MID-KVLEVART      PIC 9(6).                                    
005500*                                 LEVERERAT ANTAL STYCK                   
005600     03 MID-VKORDBTO-KOLLI   PIC Z(5)9.9.                                 
005700*                                 ORDERVIKT BRUTTO (KG)                   
005800     03 MID-VKORDBTO-4315    PIC Z(5)9.9.                                 
005900*                                 ORDERVIKT BRUTTO (KG)                   
006000*** END OF VILMAII-COPY LENGTH= 276 BYTES                                 
