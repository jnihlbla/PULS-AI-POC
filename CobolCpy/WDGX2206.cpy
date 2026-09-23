000100 01  2206-WDGX2206.                                                       
000200*                                 LEVERANTÖRSINFORMATION                  
000300*                                 PLANER VIA EDI                          
000400*                                 FYSISK NYCKEL KY2206:                   
000500*                                 (IDLEVNR + IDDC)                        
000600     03 2206-IDLEVNR         PIC X(5).                                    
000700*                                 LEVERANTÖRNUMMER                        
000800     03 2206-IDDC            PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000     03 2206-FLAVIS          PIC X.                                       
001100*                                 LEVERANTÖRSAVISERING                    
001200     03 2206-FLLEVPLP        PIC X.                                       
001300*                                 FLAGGA PERIODSLUTSSÄNDNING              
001400     03 2206-FLLEVVB         PIC X.                                       
001500*                                 FLAGGA VECKOSÄNDNING                    
001600     03 2206-FLODETTE        PIC X.                                       
001700*                                 FAKTURERING SKER VIA ODETTE             
001800     03 2206-IDLEVKND        PIC X(17).                                   
001900*                                 LEVERANTÖRENS KUNDIDENTITET             
002000     03 2206-IDOVERFNR       PIC S9(5)           COMP-3.                  
002100*                                 ÖVERFÖRINGSNUMMER                       
002200     03 2206-IDOVERFNR-VV    PIC S9(5)           COMP-3.                  
002300*                                 ÖVERFÖRINGSNUMMER                       
002400     03 2206-KDEDI           PIC X.                                       
002500*                                 ÖVERFÖRINGSSTANDARD                     
002600     03 2206-KDVECKOSL       PIC X.                                       
002700*                                 KOD    VECKOSLUTSSÄNDNING               
002800     03 2206-TISEND-SEN      PIC S9(7)           COMP-3.                  
002900*                                 SENASTE ÖVERFÖRINGSDATUM                
003000     03 FILLER               PIC X(12).                                   
003100*** END OF VILMAII-COPY LENGTH= 52 BYTES                                  
