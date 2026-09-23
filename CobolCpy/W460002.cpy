000100 01  ORAD-W460002.                                                        
000200*                                 ORDERRAD-TRANSAKTIONER NOAC             
000300*                                 POSTTYP = RHB                           
000400     03 ORAD-SORT-IDDISTR    PIC 9(4).                                    
000500*                                 DISTRIKTNUMMER                          
000600     03 ORAD-SORT-TIFILDAT   PIC 9(6).                                    
000700*                                 DATUM NÄR EN FIL SKAPATS ÅÅMMDD         
000800     03 ORAD-SORT-TIHHMMSS   PIC 9(6).                                    
000900*                                 TIM - MIN - SEK   (HHMMSS)              
001000     03 ORAD-IDPTYP          PIC X(3).                                    
001100*                                 POSTTYP                                 
001200     03 ORAD-IDDISTR         PIC 9(4).                                    
001300*                                 DISTRIKTNUMMER                          
001400     03 ORAD-IDKUNDNR        PIC 9(6).                                    
001500*                                 KUNDNUMMER                              
001600     03 ORAD-IDORDNR         PIC 9(7).                                    
001700*                                 ORDERNR             IDORDNR-002         
001800     03 ORAD-IDARTNR         PIC 9(9).                                    
001900*                                 ARTIKELNUMMER                           
002000     03 ORAD-REKSIFFR        PIC 9.                                       
002100*                                 KONTROLLSIFFRA                          
002200     03 ORAD-BERADREF        PIC X(10).                                   
002300*                                 KUNDENS RADREFERENS                     
002400     03 ORAD-KVBEART         PIC 9(6).                                    
002500*                                 BESTÄLLT ANTAL STYCKEN                  
002600     03 ORAD-KDKVBRYT        PIC 9.                                       
002700*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
002800     03 ORAD-KDDSP           PIC 9.                                       
002900*                                 PÅVERKAN PÅ DSP                         
003000     03 ORAD-TITPO           PIC 9(6).                                    
003100*                                 PLANERAD ORDERDATUM                     
003200     03 ORAD-FLSLATT         PIC X.                                       
003300*                                 FLAGGA SOM ANGER OM KVSLATT SKA         
003400*                                 LL BERÄKNAS ELLER EJ                    
003500*                                 OM FLRESTN = J OCH FLSLATT = J,         
003600*                                  DÅ BERÄKNAS KVSLATT                    
003700     03 ORAD-FLDIRLEV        PIC X.                                       
003800*                                 DIREKTLEVERANS ?                        
003900     03 ORAD-FILLER          PIC X.                                       
004000     03 ORAD-REKSIFFR-RAETT  PIC 9.                                       
004100*                                 KONTROLLSIFFRA                          
004200     03 ORAD-IDTRANSLOP      PIC 9(5).                                    
004300*                                 TRANSAKTIONS-LÖPNUMMER                  
004400     03 ORAD-KDFEL           PIC 9(3).                                    
004500*                                 FELKOD                                  
004600     03 ORAD-DEAL-PR-LINE.                                                
004700*                                 DEALERPRIS (RAD)                        
004800        05 ORAD-IDPRQUES     PIC 9(7).                                    
004900*                                 PRISFRÅGA NR                            
005000        05 ORAD-PRARTNTO-LOC PIC S9(7)V9(2)      COMP-3.                  
005100*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
005200        05 ORAD-PRARTNTO-LOCPREL                                          
005300                             PIC S9(7)V9(2)      COMP-3.                  
005400*                                 PREL NETTO SLUTKUNDSPRIS I              
005500*                                 LOKAL VALUTA                            
005600        05 ORAD-PRARTBTO-LOC PIC S9(7)V9(2)      COMP-3.                  
005700*                                 PRIS I LOKAL VALUTA                     
005800        05 ORAD-KDVALISO     PIC X(3).                                    
005900*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
006000        05 ORAD-KDVAT        PIC X(2).                                    
006100*                                 MOMSKOD                                 
006200        05 ORAD-RERAB        PIC S9(2)V9(1)      COMP-3.                  
006300*                                 RABATTSATS (PROCENT)                    
006400        05 ORAD-KDRAB        PIC X(5).                                    
006500*                                 RABATTKOD                               
006600        05 ORAD-BEART-VIPS   PIC X(25).                                   
006700*                                 VIPS ARTIKELBENÄMNING                   
006800*                                 PÅ DEALERNS SPRÅK                       
006900*** END OF VILMAII-COPY LENGTH= 141 BYTES                                 
