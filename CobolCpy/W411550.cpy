000100 01  W411550.                                                             
000200*                                 TYP = 550, LÄNGD = 87                   
000300*                                                                         
000400     03 IDTYP                PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 IDDISTR              PIC 9(4).                                    
000700*                                 DISTRIKTNUMMER                          
000800     03 IDKUNDNR             PIC 9(6).                                    
000900*                                 KUNDNUMMER                              
001000     03 KDCLAGER             PIC 9.                                       
001100*                                 CENTRALLAGERKOD                         
001200     03 KDFRAKT              PIC 9(2).                                    
001300*                                 FRAKTSÄTT DC TILL KUND                  
001400     03 IDORDNR              PIC 9(5).                                    
001500*                                 ORDERNUMMER UTGÅR PD90                  
001600     03 IDARTNR              PIC 9(9).                                    
001700*                                 ARTIKELNUMMER                           
001800     03 REKSIFFR             PIC 9.                                       
001900*                                 KONTROLLSIFFRA                          
002000     03 KVBEART              PIC 9(6).                                    
002100*                                 BESTÄLLT ANTAL STYCKEN                  
002200     03 KDKVBRYT             PIC 9.                                       
002300*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
002400     03 KVLEVART             PIC 9(6).                                    
002500*                                 LEVERERAT ANTAL STYCK                   
002600     03 FILLER REDEFINES KVLEVART.                                        
002700        05 FILLER            PIC X(3).                                    
002800        05 TIINLEV           PIC 9(3).                                    
002900*                                 INLEVERANS-VECKA                        
003000     03 PRARTNTO             PIC 9(9).                                    
003100*                                 ARTIKELPRIS NETTO                       
003200     03 FILLER REDEFINES PRARTNTO.                                        
003300        05 REANTPSA          PIC 9(6)V9(3).                               
003400*                                 ANTAL PER SATS     REANTPSA-002         
003500     03 IDKONTO              PIC 9(10).                                   
003600*                                 KONTO                                   
003700     03 IDKST                PIC X(10).                                   
003800*                                 KOSTNADSSTÄLLE                          
003900     03 KDORDKL              PIC 9.                                       
004000*                                 ORDERKLASS                              
004100     03 FLEJKRED             PIC 9.                                       
004200*                                 EJ KREDITERING                          
004300     03 FILLER               PIC X(9).                                    
004400     03 IDRADNR-VO           PIC 9(2).                                    
004500*                                                  IDRADNR-VO-003         
004600*                                 RADNUMMER I VOLVOORDER                  
004700     03 KDMASK               PIC X.                                       
004800*                                 KOD FÖR MASKINELL ORDERRAD              
004900*                                 H = SATS            W221                
005000*                                 R = RO/DO           W441                
005100*                                 S = SATS            R241                
005200*                                 D = ORDERRAD        D886                
005300*                                 B = BIPACKAD RO/DO  W411                
005400*** END OF VILMAII-COPY LENGTH= 87 BYTES                                  
