000100 01  2216-WDGX2216.                                                       
000200*                                 LEVERANTÖRSINFORMATION                  
000300*                                 FYSISK NYCKEL IDLEVNR                   
000400     03 2216-IDLEVNR         PIC X(5).                                    
000500*                                 LEVERANTÖRNUMMER                        
000600     03 2216-IDOVERFNR       PIC S9(5)           COMP-3.                  
000700*                                 ÖVERFÖRINGSNUMMER                       
000800     03 2216-TISEND-BEG      PIC S9(7)           COMP-3.                  
000900*                                 BEGÄRD ÖVERFÖRINGSDATUM                 
001000     03 2216-TISEND-SEN      PIC S9(7)           COMP-3.                  
001100*                                 SENASTE ÖVERFÖRINGSDATUM                
001200     03 2216-KDVECKOSL       PIC X.                                       
001300*                                 KOD    VECKOSLUTSSÄNDNING               
001400     03 2216-KDEDI           PIC X.                                       
001500*                                 ÖVERFÖRINGSSTANDARD                     
001600     03 2216-FLAVIS          PIC X.                                       
001700*                                 LEVERANTÖRSAVISERING                    
001800     03 2216-IDOVERFNR-VV    PIC S9(5)           COMP-3.                  
001900*                                 ÖVERFÖRINGSNUMMER                       
002000     03 FILLER               PIC X.                                       
002100     03 2216-IDLEVKND        PIC X(17).                                   
002200*                                 LEVERANTÖRENS KUNDIDENTITET             
002300     03 2216-FLLEVPLP        PIC X.                                       
002400*                                 FLAGGA PERIODSLUTSSÄNDNING              
002500     03 2216-TISEND-PER      PIC S9(7)           COMP-3.                  
002600*                                 BEGÄRD ÖVERFÖRINGSDATUM                 
002700     03 2216-FLODETTE        PIC X.                                       
002800*                                 FAKTURERING SKER VIA ODETTE             
002900     03 2216-FLLEVVB         PIC X.                                       
003000*                                 FLAGGA VECKOSÄNDNING                    
003100     03 FILLER               PIC X(12).                                   
003200*** END OF VILMAII-COPY LENGTH= 59 BYTES                                  
