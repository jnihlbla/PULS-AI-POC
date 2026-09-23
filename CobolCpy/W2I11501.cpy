000100 01  MID-W2I11501.                                                        
000200*                                 MID-COPYTEXT FÖR W211500                
000300     03 MID-IDLEVNR-IN       PIC X(5).                                    
000400*                                 LEVERANTÖRNUMMER                        
000500     03 MID-IDLEVNR-UT       PIC X(5).                                    
000600*                                 LEVERANTÖRNUMMER                        
000700     03 MID-IDATTENT-ENTER   PIC 9(2).                                    
000800*                                 ATTENTION NUMMER                        
000900     03 MID-IDATTENT-NEXT    PIC 9(2).                                    
001000*                                 ATTENTION NUMMER                        
001100     03 MID-INPUT.                                                        
001200*                                                                         
001300        05 MID-INPUT-ADR.                                                 
001400*                                                                         
001500           07 MID-BELEV-VCC  PIC X(60).                                   
001600*                                 LEVERANTÖRSNAMN VCC                     
001700           07 MID-ADLEV-RAD1 PIC X(35).                                   
001800*                                 LEVERANTÖRENS GATUADRESS                
001900           07 MID-ADLEV-RAD2-VCC                                          
002000                             PIC X(42).                                   
002100*                                 LEVERANTÖRSADRESS RAD2 VCC              
002200           07 MID-ADLEV-ORT-VCC                                           
002300                             PIC X(46).                                   
002400*                                 LEVERANTÖRSADRESS ORT VCC               
002500           07 MID-ADLEVLND   PIC X(20).                                   
002600*                                 LEVERANTÖRSADRESS LAND                  
002700           07 MID-IDLEVTLF   PIC X(20).                                   
002800*                                 TELEFONNUMMER TILL LEVERANTÖR           
002900           07 MID-IDLEVFAX   PIC X(20).                                   
003000*                                 TELEFAXNUMMER TILL LEVERANTÖR           
003100           07 MID-IDLANDX2   PIC X(2).                                    
003200*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
003300        05 MID-KVVECKOR-LT   PIC X(2).                                    
003400*                                 ANTAL VECKOR LEDTID                     
003500        05 MID-KVVECKOR-AT   PIC X(2).                                    
003600*                                 ANTAL VECKOR ANSKAFFNINGSTID            
003700        05 MID-KDLEVTYP      PIC 9.                                       
003800*                                 LEVERANTÖRTYP                           
003900        05 MID-FLRSADR       PIC X.                                       
004000*                                 RS-UNIK LEV-ADRESS                      
004100        05 MID-IDATTENT-IN   PIC X(2).                                    
004200*                                 ATTENTION NUMMER                        
004300     03 MID-BELEV-UPD        PIC X(35).                                   
004400*                                 LEVERANTÖRSNAMN                         
004500     03 MID-IDMAIL-UPD       PIC X(50).                                   
004600*                                 MAIL ADRESS                             
004700     03 MID-IDLEVTLF-KLEV-UPD                                             
004800                             PIC X(20).                                   
004900*                                 TEL KONTAKTPERS HOS LEVERANTÖR          
005000     03 MID-TENOTE-UPD       PIC X(35).                                   
005100     03 MID-FLAGGA-BORT      PIC X.                                       
005200*                                 ALLMÄN SVARSFLAGGA                      
005300     03 MID-IDATTENT-UPD     PIC 9(2).                                    
005400*                                 ATTENTION NUMMER                        
005500*** END OF VILMAII-COPY LENGTH= 410 BYTES                                 
