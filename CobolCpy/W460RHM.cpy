000100 01  RHM-W460RHM-CTX.                                                     
000200*                                 EXCHANGE INFO FROM VIPS TO NOAC         
000300*                                  (SCREEN 3171-3172)                     
000400*                                                                         
000500     03 RHM-IDPTYP           PIC X(3).                                    
000600*                                 POSTTYP                                 
000700     03 RHM-IDGMTREF.                                                     
000800*                                 GODSMOTTAGAREREFERENS                   
000900        05 RHM-IDDISTR       PIC S9(5)           COMP-3.                  
001000*                                 DISTRIKTNUMMER                          
001100        05 RHM-IDKUNDNR      PIC S9(7)           COMP-3.                  
001200*                                 KUNDNUMMER                              
001300        05 RHM-IDKUNDRF      PIC X(10).                                   
001400*                                 KUNDENS REFERENS (ORDERID)              
001500        05 RHM-IDORDNR5-FILLER REDEFINES RHM-IDKUNDRF.                    
001600           07 RHM-IDORDNR5   PIC 9(5).                                    
001700*                                 ORDERNUMMER                             
001800           07 FILLER         PIC X(5).                                    
001900        05 RHM-IDORDNR7-FILLER REDEFINES RHM-IDKUNDRF.                    
002000           07 RHM-IDORDNR7   PIC 9(7).                                    
002100*                                 ORDERNUMMER                             
002200           07 FILLER         PIC X(3).                                    
002300     03 RHM-IDFAKT           PIC S9(7)           COMP-3.                  
002400*                                 FAKTURANUMMER                           
002500     03 RHM-IDBYTRAP         PIC S9(7)           COMP-3.                  
002600*                                 RAPPORTNUMMER  BYTES                    
002700     03 RHM-TIREGDAT         PIC S9(7)           COMP-3.                  
002800*                                 REGISTRERINGSDATUM (≈≈MMDD)             
002900     03 RHM-IDARTNR-OBJ      PIC S9(9)           COMP-3.                  
003000*                                 OBJEKTNUMMER                            
003100     03 RHM-IDTABNR          PIC S9(3)           COMP-3.                  
003200*                                 TABELLNUMMER                            
003300     03 RHM-KVRETUR-URSP     PIC S9(5)           COMP-3.                  
003400*                                 ANTAL I RETUR                           
003500     03 RHM-BERADREF         PIC X(10).                                   
003600*                                 KUNDENS RADREFERENS                     
003700     03 RHM-IDDC             PIC X(2).                                    
003800*                                 IDENTIFIERARE LAGER                     
003900     03 RHM-IDBYTRAD         PIC S9(5)           COMP-3.                  
004000*                                 RADNUMMER                               
004100     03 RHM-FLBYTGAR         PIC X.                                       
004200*                                 GARANTI RAPPORT FLAGGA                  
004300*                                 Y = GARANTI                             
004400*                                 N = EJ GARANTI                          
004500     03 RHM-FILLERX22        PIC X(22).                                   
004600*** END OF VILMAII-COPY LENGTH= 80 BYTES                                  
