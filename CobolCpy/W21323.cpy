000100 01  W21323.                                                              
000200*                                 NEDLÄST    LEVERANTÖRSREGISTER          
000300*                                                                         
000400     03 IDLEVNR              PIC X(5).                                    
000500*                                 LEVERANTÖRNUMMER                        
000600     03 ADRESS.                                                           
000700*                                                                         
000800        05 BELEV             PIC X(35).                                   
000900*                                 LEVERANTÖRSNAMN                         
001000        05 ADLEV-RAD1        PIC X(35).                                   
001100*                                 LEVERANTÖRENS GATUADRESS                
001200        05 ADLEV-RAD2        PIC X(35).                                   
001300*                                 LEVERANTÖRENS GATUADRESS 2              
001400        05 ADLEV-ORT         PIC X(35).                                   
001500*                                 LEVERANTÖRSADRESS ORT                   
001600        05 ADLEVLND          PIC X(20).                                   
001700*                                 LEVERANTÖRSADRESS LAND                  
001800        05 IDLEVTLF          PIC X(20).                                   
001900*                                 TELEFONNUMMER TILL LEVERANTÖR           
002000     03 ATTENT               OCCURS 2 TIMES.                              
002100*                                                                         
002200        05 IDATTENT          PIC S9(3)           COMP-3.                  
002300*                                 ATTENTION NUMMER                        
002400        05 ATT-BELEV         PIC X(35).                                   
002500*                                 LEVERANTÖRSNAMN                         
002600        05 IDLEVTLF-KLEV     PIC X(20).                                   
002700*                                 TEL KONTAKTPERS HOS LEVERANTÖR          
002800        05 IDMAIL            PIC X(50).                                   
002900*                                 MAIL ADRESS                             
003000        05 TENOTE            PIC X(40).                                   
003100*                                 NOTERINGSFÄLT                           
003200*** END OF VILMAII-COPY LENGTH= 479 BYTES                                 
