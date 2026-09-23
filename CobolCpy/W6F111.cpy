000100 01  GADR-W6F111.                                                         
000200*                                 LEVERANTÖRSREGISTER                     
000300*                                 KVALITET                                
000400*                                 GODSADRESS TILL LEVERANTÖR              
000500*                                 FYSISK NYCKEL: IDLEVG                   
000600     03 GADR-IDLEVG          PIC S9(5)           COMP-3.                  
000700*                                 LEVERANTÖRS GODSADRESS NUMMER           
000800*                                 SUPPLIER WAREHOUSE NUMBER               
000900     03 GADR-BELEV           PIC X(35).                                   
001000*                                 LEVERANTÖRSNAMN                         
001100*                                 SUPPLIER NAME                           
001200     03 GADR-ADLEV-RAD1      PIC X(35).                                   
001300*                                 LEVERANTÖRENS GATUADRESS                
001400*                                 SUPPLIER ADDRESS STREET                 
001500     03 GADR-ADLEV-RAD2      PIC X(35).                                   
001600*                                 LEVERANTÖRENS GATUADRESS 2              
001700*                                 SUPPLIER ADDRESS STREET                 
001800     03 GADR-ADLEV-ORT       PIC X(35).                                   
001900*                                 LEVERANTÖRSADRESS ORT                   
002000*                                 SUPPLIER ADDRESS                        
002100     03 GADR-ADLEVLND        PIC X(20).                                   
002200*                                 LEVERANTÖRSADRESS LAND                  
002300*                                 SUPPLIER COUNTRY ADDRESS                
002400     03 GADR-IDLEVTLF        PIC X(20).                                   
002500*                                 TELEFONNUMMER TILL LEVERANTÖR           
002600*                                 TELEPHONE NUMBER TO SUPPLIER            
002700     03 GADR-ADATTENT        OCCURS 2 TIMES                               
002800                             PIC X(40).                                   
002900*                                 ATTENTIONADRESS                         
003000*                                 ATTENTION ADDRESS                       
003100     03 GADR-FILLER          PIC X(12).                                   
003200*** END OF VILMAII-COPY LENGTH= 275 BYTES                                 
