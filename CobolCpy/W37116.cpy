000100 01  W37116.                                                              
000200*                                 FIL FRÅN VIPS                           
000300*                                 OBJEKT FÖR GODKÄNNANDE PÅ BILD          
000400*                                 3171-3172                               
000500     03 IDPTYP               PIC X(3).                                    
000600*                                 POSTTYP                                 
000700     03 IDGMTREF.                                                         
000800*                                 GODSMOTTAGAREREFERENS                   
000900        05 IDDISTR           PIC S9(5)           COMP-3.                  
001000*                                 DISTRIKTNUMMER                          
001100        05 IDKUNDNR          PIC S9(7)           COMP-3.                  
001200*                                 KUNDNUMMER                              
001300        05 IDKUNDRF-GRP.                                                  
001400*                                 KUNDENS REFERENS (ORDERID)              
001500           07 IDKUNDRF       PIC X(10).                                   
001600*                                 KUNDENS REFERENS (ORDERID)              
001700           07 IDORDNR5-FILLER REDEFINES IDKUNDRF.                         
001800              09 IDORDNR5    PIC 9(5).                                    
001900*                                 ORDERNUMMER                             
002000              09 FILLER      PIC X(5).                                    
002100           07 IDORDNR7-FILLER REDEFINES IDKUNDRF.                         
002200              09 IDORDNR7    PIC 9(7).                                    
002300*                                 ORDERNUMMER                             
002400              09 FILLER      PIC X(3).                                    
002500     03 IDFAKT               PIC S9(7)           COMP-3.                  
002600*                                 FAKTURANUMMER                           
002700     03 IDBYTRAP             PIC S9(7)           COMP-3.                  
002800*                                 RAPPORTNUMMER  BYTES                    
002900     03 IDBYTRAD             PIC S9(5)           COMP-3.                  
003000*                                 RADNUMMER                               
003100     03 TIREGDAT             PIC S9(7)           COMP-3.                  
003200*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
003300     03 IDARTNR-OBJ          PIC S9(9)           COMP-3.                  
003400*                                 OBJEKTNUMMER                            
003500     03 IDTABNR              PIC S9(3)           COMP-3.                  
003600*                                 TABELLNUMMER                            
003700     03 KVRETUR-URSP         PIC S9(5)           COMP-3.                  
003800*                                 ANTAL I RETUR                           
003900     03 BERADREF             PIC X(10).                                   
004000*                                 KUNDENS RADREFERENS                     
004100     03 IDDC                 PIC X(2).                                    
004200*                                 IDENTIFIERARE LAGER                     
004300     03 FLBYTGAR             PIC X.                                       
004400*                                 GARANTI RAPPORT FLAGGA                  
004500*                                 Y = GARANTI                             
004600*                                 N = EJ GARANTI                          
004700     03 IDDISTR-FEL          PIC 9(4).                                    
004800*                                 DISTRIKTNUMMER                          
004900*** END OF VILMAII-COPY LENGTH= 62 BYTES                                  
