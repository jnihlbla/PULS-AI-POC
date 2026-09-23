000100 01  MID-W4I39001.                                                        
000200*                                 KOMMENTAR (SVENSKA)                     
000300*                                 31 POS / RAD                            
000400     03 MID-IDANSTNR         PIC X(5).                                    
000500*                                 ANSTÄLLNINGSNUMMER                      
000600     03 MID-IDDISTR          PIC X(4).                                    
000700*                                 DISTRIKTNUMMER                          
000800     03 MID-IDKUNDNR         PIC X(6).                                    
000900*                                 KUNDNUMMER                              
001000     03 MID-IDORDNR          PIC X(5).                                    
001100*                                 ORDERNUMMER UTGÅR PD90                  
001200     03 MID-IDPRODNR         PIC X(7).                                    
001300*                                 PRODUKTIONSNUMMER                       
001400     03 MID-IDDC             PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600     03 MID-IDKOLLI          PIC X(5).                                    
001700*                                 KOLLINUMMER                             
001800     03 MID-IDSUPREF         PIC X(10).                                   
001900*                                 LEVERANTöRSREF.                         
002000     03 MID-DASUPREF         PIC X(8).                                    
002100*                                 SÄNDNINGSDATUM DIREKTLEVERANTÖR         
002200     03 MID-TISUPTID         PIC X(5).                                    
002300*                                 SÄNDNINGSTID DIREKTLEVERANTÖR           
002400     03 MID-VKORDBTO-KOLLI   PIC X(8).                                    
002500*                                 ORDERVIKT BRUTTO PER KOLLI              
002600     03 MID-KDEMBTYP         PIC X.                                       
002700*                                 EMBALLAGETYP                            
002800     03 MID-DIKOLLIL         PIC X(4).                                    
002900*                                 KOLLI-LÄNGD                             
003000     03 MID-DIKOLLIB         PIC X(3).                                    
003100*                                 KOLLI-BREDD                             
003200     03 MID-DIKOLLIH         PIC X(3).                                    
003300*                                 KOLLI-HÖJD                              
003400     03 MID-FLSLUT           PIC X.                                       
003500*                                 AVSLUTNINGSFLAGGA                       
003600     03 MID-FLSLUT-VORD      PIC X.                                       
003700*                                 AVSLUTNINGSFLAGGA                       
003800     03 MID-VLORDBTO-KOLLI   PIC X(8).                                    
003900*                                 ORDERVOLYM BRUTTO KOLLI                 
004000     03 MID-RAD              OCCURS 74 TIMES.                             
004100        05 MID-IDRADNR       PIC X(4).                                    
004200*                                 RADNUMMER                               
004300        05 MID-KVLEVART      PIC X(6).                                    
004400*                                 LEVERERAT ANTAL STYCK                   
004500        05 MID-KDARTURS      PIC X(2).                                    
004600*                                 ARTIKELURSPRUNGSKOD                     
004700*** END OF VILMAII-COPY LENGTH= 974 BYTES                                 
