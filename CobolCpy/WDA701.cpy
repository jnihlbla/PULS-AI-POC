000100 01  ROT-WDA701.                                                          
000200*                                 ROTSEGMENT BYTES-REGISTER               
000300*                                 EJ AVSLUTADE TRANSAKTIONER              
000400*                                 FYSISK NYCKEL WDA701KY                  
000500*                                  ( IDPTYP + IDDISTR + )                 
000600*                                  ( IDARTNR, IDKUNDNR + )                
000700*                                  ( IDBYTRAD            )                
000800     03 ROT-IDPTYP           PIC X(3).                                    
000900*                                 POSTTYP                                 
001000     03 ROT-IDDISTR          PIC S9(5)           COMP-3.                  
001100*                                 DISTRIKTNUMMER                          
001200     03 ROT-IDARTNR-BYT      PIC S9(9)           COMP-3.                  
001300*                                 ARTIKELNUMMER FÖR BYTES                 
001400     03 ROT-IDKUNDNR         PIC S9(7)           COMP-3.                  
001500*                                 KUNDNUMMER                              
001600     03 ROT-IDBYTRAD         PIC S9(5)           COMP-3.                  
001700*                                 RADNUMMER                               
001800     03 ROT-DAFAKT           PIC 9(8).                                    
001900*                                 FAKTURERINGSDATUM (ÅÅÅÅMMDD)            
002000     03 ROT-DAREGDAT         PIC 9(8).                                    
002100*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
002200     03 ROT-IDDC             PIC X(2).                                    
002300*                                 IDENTIFIERARE LAGER                     
002400     03 ROT-IDORDER          PIC S9(7)           COMP-3.                  
002500*                                 VOLVO PARTS ORDERNUMMER                 
002600     03 ROT-IDUSER           PIC X(8).                                    
002700*                                 ANVÄNDARENS SÄKERHETS ID                
002800     03 ROT-KDEXCHA          PIC S9(3)           COMP-3.                  
002900*                                 EXCHANGE ACCOUNT CODE                   
003000     03 ROT-KVANTAL          PIC S9(7)           COMP-3.                  
003100*                                 ANTAL                                   
003200     03 ROT-KVPOINT          PIC S9(7)           COMP-3.                  
003300*                                 POINT VALUE                             
003400     03 ROT-TIKLOCK          PIC S9(9)           COMP-3.                  
003500*                                 KLOCKSLAG (TTMMSSTH)                    
003600     03 ROT-TENOTE           PIC X(40).                                   
003700*                                 NOTERINGSFÄLT                           
003800     03 ROT-FILLER           PIC X(16).                                   
003900*** END OF VILMAII-COPY LENGTH= 119 BYTES                                 
