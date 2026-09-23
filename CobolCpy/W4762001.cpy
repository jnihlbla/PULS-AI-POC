000100 01  RSI-W4762001.                                                        
000200*                                 INFO OM SKEPPNING, DDI MARKNAD          
000300*                                 PT-RSI                                  
000400     03 RSI-IDPTYP           PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 RSI-IDDC             PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800     03 RSI-IDSHIPM          PIC 9(7).                                    
000900*                                 SKEPPNINGSNUMMER                        
001000     03 RSI-TISKEPPN         PIC 9(6).                                    
001100*                                 SKEPPNINGSDATUM  (ÅÅMMDD)               
001200     03 RSI-IDDISTR          PIC 9(4).                                    
001300*                                 DISTRIKTNUMMER                          
001400     03 RSI-IDKUNDNR         PIC 9(6).                                    
001500*                                 KUNDNUMMER                              
001600     03 RSI-IDORDNR7         PIC 9(7).                                    
001700*                                 ORDERNUMMER                             
001800     03 RSI-FLCOD            PIC X.                                       
001900*                                 KONTANTBETALANDE KUND                   
002000     03 RSI-KDORDKL          PIC 9.                                       
002100*                                 ORDERKLASS                              
002200     03 RSI-IDKOLLI          PIC 9(5).                                    
002300*                                 KOLLINUMMER                             
002400     03 RSI-IDARTNR          PIC 9(9).                                    
002500*                                 ARTIKELNUMMER                           
002600     03 RSI-REKSIFFR         PIC 9.                                       
002700*                                 KONTROLLSIFFRA                          
002800     03 RSI-PRARTNTO-LOC     PIC 9(7)V9(2).                               
002900*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
003000     03 RSI-FLARTSTD         PIC X.                                       
003100*                                 INDIKERAR PRARTSTD                      
003200     03 RSI-KVBEART          PIC 9(6).                                    
003300*                                 BESTÄLLT ANTAL STYCKEN                  
003400     03 RSI-KVLEVART         PIC 9(7).                                    
003500*                                 LEVERERAT ANTAL STYCK                   
003600     03 RSI-KDDSP            PIC 9.                                       
003700*                                 PÅVERKAN PÅ DSP                         
003800     03 RSI-BERADREF         PIC X(10).                                   
003900*                                 KUNDENS RADREFERENS                     
004000     03 RSI-IDRONR           PIC 9(5).                                    
004100*                                 RESTORDERNUMMER                         
004200     03 RSI-FLIHOP           PIC X.                                       
004300*                                 JA/NEJ-FLAGGA                           
004400     03 RSI-BEVOLREF         PIC X(10).                                   
004500*                                 VOLVO REFERENS                          
004600     03 RSI-IDPRODNR         PIC S9(7)           COMP-3.                  
004700*                                 PRODUKTIONSNUMMER                       
004800     03 RSI-IDPURAD          PIC S9(5)           COMP-3.                  
004900*                                 RADNUMMER PÅ PACKUNDERLAG               
005000     03 RSI-FILLER           PIC X(3).                                    
005100*** END OF VILMAII-COPY LENGTH= 112 BYTES                                 
