000100 01  ADR-WDF106.                                                          
000200*                                 LEVERANTÖRSREGISTER                     
000300*                                 ADRESSINFO OM LEVERANTÖR                
000400*                                 FYSISK NYCKEL  IDLEVSUF                 
000500     03 ADR-IDLEVSUF         PIC S9              COMP-3.                  
000600*                                 LEVERANTÖRSSUFFIX                       
000700*                                 SUPPLIER SUFFIX                         
000800     03 ADR-BELEV-VCC        PIC X(60).                                   
000900*                                 LEVERANTÖRSNAMN VCC                     
001000*                                 SUPPLIER NAME VCC                       
001100     03 ADR-BELEV-FILLER REDEFINES ADR-BELEV-VCC.                         
001200        05 ADR-BELEV         PIC X(35).                                   
001300*                                 LEVERANTÖRSNAMN                         
001400*                                 SUPPLIER NAME                           
001500        05 FILLER            PIC X(25).                                   
001600     03 ADR-ADLEV-RAD1       PIC X(35).                                   
001700*                                 LEVERANTÖRENS GATUADRESS                
001800*                                 SUPPLIER ADDRESS STREET                 
001900     03 ADR-ADLEV-RAD2-VCC   PIC X(42).                                   
002000*                                 LEVERANTÖRSADRESS RAD2 VCC              
002100*                                 SUPPLIER ADDRESS LINE                   
002200     03 ADR-ADLEV-RAD2-FILLER REDEFINES ADR-ADLEV-RAD2-VCC.               
002300        05 ADR-ADLEV-RAD2    PIC X(35).                                   
002400*                                 LEVERANTÖRENS GATUADRESS 2              
002500*                                 SUPPLIER ADDRESS STREET                 
002600        05 FILLER            PIC X(7).                                    
002700     03 ADR-ADLEV-ORT-VCC    PIC X(46).                                   
002800*                                 LEVERANTÖRSADRESS ORT VCC               
002900*                                 SUPPLIER ADDRESS LINE                   
003000     03 ADR-ADLEV-ORT-FILLER REDEFINES ADR-ADLEV-ORT-VCC.                 
003100        05 ADR-ADLEV-ORT     PIC X(35).                                   
003200*                                 LEVERANTÖRSADRESS ORT                   
003300*                                 SUPPLIER ADDRESS                        
003400        05 FILLER            PIC X(11).                                   
003500     03 ADR-ADLEVLND         PIC X(20).                                   
003600*                                 LEVERANTÖRSADRESS LAND                  
003700*                                 SUPPLIER COUNTRY ADDRESS                
003800     03 ADR-IDLEVTLF         PIC X(20).                                   
003900*                                 TELEFONNUMMER TILL LEVERANTÖR           
004000*                                 TELEPHONE NUMBER TO SUPPLIER            
004100     03 ADR-IDLEVTLX         PIC X(20).                                   
004200*                                 TELEXNUMMER TILL LEVERANTÖR             
004300*                                 TELEX NUMBER TO THE SUPPLIER            
004400     03 ADR-IDLEVFAX         PIC X(20).                                   
004500*                                 TELEFAXNUMMER TILL LEVERANTÖR           
004600*                                 TELEFAX NUMBER TO THE SUPPLIER          
004700     03 ADR-IDLANDX2         PIC X(2).                                    
004800*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
004900*                                 2-LETTER CODE FOR COUNTRY               
005000     03 ADR-IDVAT            PIC X(17).                                   
005100*                                 MOMSREGISTRERINGSNUMMER                 
005200*                                 VAT REGISTRATION NUMBER                 
005300*** END OF VILMAII-COPY LENGTH= 283 BYTES                                 
