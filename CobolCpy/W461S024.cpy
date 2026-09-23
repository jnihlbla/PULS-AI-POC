000100 01  RKE-W461S024-CTX.                                                    
000200*                                 SAMMANSLAGNING AV SORTDEL OCH           
000300*                                 KREDIT   POST (RKE) TILL NOAC           
000400     03 RKE-SOR0-IDDISTR     PIC S9(5)           COMP-3.                  
000500*                                 DISTRIKTNUMMER                          
000600     03 RKE-SOR0-IDKUNDNR    PIC S9(7)           COMP-3.                  
000700*                                 KUNDNUMMER                              
000800     03 RKE-SOR0-IDRONR      PIC S9(7)           COMP-3.                  
000900*                                 RESTORDERNUMMER      IDRONR-002         
001000     03 RKE-SOR0-TIRODAT     PIC S9(7)           COMP-3.                  
001100*                                 RESTORDERDATUM         (≈≈MMDD)         
001200     03 RKE-SOR0-IDPTYP      PIC X(3).                                    
001300*                                 POSTTYP                                 
001400     03 RKE-SOR0-IDLOPNR     PIC S9(5)           COMP-3.                  
001500*                                 L÷PNUMMER          IDLOPNR-002          
001600     03 RKE-W461RKEN-CTX.                                                 
001700*                                 KREDITTRANS-RAD                         
001800*                                 RETURTILLST≈NDSTRANS                    
001900*                                 RECORD TYP  RKE                         
002000        05 RKE-IDPTYP        PIC X(3).                                    
002100*                                 POSTTYP                                 
002200        05 RKE-IDDISTR       PIC 9(4).                                    
002300*                                 DISTRIKTNUMMER                          
002400        05 RKE-IDKUNDNR      PIC 9(6).                                    
002500*                                 KUNDNUMMER                              
002600        05 RKE-IDDC          PIC X(2).                                    
002700*                                 IDENTIFIERARE LAGER                     
002800        05 RKE-IDRAPPNR      PIC 9(7).                                    
002900*                                 RAPPORT NUMMER                          
003000        05 RKE-IDRADNR       PIC 9(4).                                    
003100*                                 RADNUMMER                               
003200        05 RKE-IDARTNR       PIC 9(9).                                    
003300*                                 ARTIKELNUMMER                           
003400        05 RKE-REKSIFFR      PIC 9.                                       
003500*                                 KONTROLLSIFFRA                          
003600        05 RKE-TIRETILL      PIC 9(6).                                    
003700*                                 RETURTILLST≈NDSDATUM                    
003800        05 RKE-IDRAPPNR-002  PIC 9(7).                                    
003900*                                 RAPPORT NUMMER                          
004000        05 RKE-FILLERX31     PIC X(31).                                   
004100*** END OF VILMAII-COPY LENGTH= 101 BYTES                                 
