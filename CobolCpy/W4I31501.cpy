000100 01  MID-W4I31501.                                                        
000200*                                 KOMMENTAR (SVENSKA)                     
000300*                                 31 POS / RAD                            
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
003400     03 MID-KDKOLLI          PIC X(8).                                    
003500*                                 KOLLIKOD                                
003600     03 MID-VKORDBTO-KOLLI   PIC 9(7).                                    
003700*                                 ORDERVIKT BRUTTO (KG)                   
003800     03 MID-FLSISTAK         PIC X.                                       
003900*                                 SISTA KOLLI I ORDERN?                   
004000     03 MID-KDEMBTYP         PIC 9.                                       
004100*                                 EMBALLAGETYP                            
004200     03 MID-DIKOLLIL         PIC 9(4).                                    
004300*                                 KOLLI-LÄNGD                             
004400     03 MID-DIKOLLIB         PIC 9(3).                                    
004500*                                 KOLLI-BREDD                             
004600     03 MID-DIKOLLIH         PIC 9(3).                                    
004700*                                 KOLLI-HÖJD                              
004800     03 MID-ADFLGEO          PIC X(3).                                    
004900*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
005000     03 MID-ADFLOMR          PIC X(3).                                    
005100*                                 LASTNINGSOMR/STÄLL FÄRDIGLAGER          
005200     03 MID-ADRUTNIV         PIC X(3).                                    
005300*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
005400     03 MID-IDKOLLI-FOM      PIC X(5).                                    
005500*                                 KOLLINUMMER                             
005600     03 MID-IDKOLLI-TOM      PIC X(5).                                    
005700*                                 KOLLINUMMER                             
005800     03 MID-PRTVAL-ADRESSFL  PIC X(2).                                    
005900*                                 PRINTER-VAL KOD ADRESS FLAGGA           
006000     03 MID-PRTVAL-FOLJEFL   PIC X(2).                                    
006100*                                 PRINTER-VAL KOD FÖLJESEDEL              
006200     03 MID-RAD              OCCURS 12 TIMES.                             
006300        05 MID-IDRADNR-FOM   PIC X(4).                                    
006400*                                 RADNUMMER                               
006500        05 MID-IDRADNR-TOM   PIC X(4).                                    
006600*                                 RADNUMMER                               
006700        05 MID-KVLEVART      PIC X(6).                                    
006800*                                 LEVERERAT ANTAL STYCK                   
006900*** END OF VILMAII-COPY LENGTH= 290 BYTES                                 
