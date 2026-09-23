000100 01  W21320X.                                                             
000200     03 IDLEVNR              PIC X(5)                                     
000300                             VALUE SPACES.                                
000400*                                 LEVERANTÖRNUMMER                        
000500*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
000600     03 KVDAGAR-TTC1         PIC Z9                                       
000700                             VALUE ZEROS.                                 
000800*                                 DAGAR TULL- OCH TRANSPORT-TID           
000900*                                 C1                                      
001000     03 KVVECKOR-LT          PIC Z9                                       
001100                             VALUE ZEROS.                                 
001200*                                 ANTAL VECKOR LEDTID                     
001300     03 KVVECKOR-AT          PIC Z9                                       
001400                             VALUE ZEROS.                                 
001500*                                 ANTAL VECKOR ANSKAFFNINGSTID            
001600     03 BELEV                PIC X(35)                                    
001700                             VALUE SPACES.                                
001800*                                 LEVERANTÖRSNAMN                         
001900*                                 SUPPLIER NAME                           
002000     03 ADLEV-RAD1           PIC X(35)                                    
002100                             VALUE SPACES.                                
002200*                                 LEVERANTÖRENS GATUADRESS                
002300*                                 SUPPLIER ADDRESS STREET                 
002400     03 ADLEV-RAD2           PIC X(35)                                    
002500                             VALUE SPACES.                                
002600*                                 LEVERANTÖRENS GATUADRESS 2              
002700*                                 SUPPLIER ADDRESS STREET                 
002800     03 ADLEV-ORT            PIC X(35)                                    
002900                             VALUE SPACES.                                
003000*                                 LEVERANTÖRSADRESS ORT                   
003100*                                 SUPPLIER ADDRESS                        
003200     03 ADLEVLND             PIC X(20)                                    
003300                             VALUE SPACES.                                
003400*                                 LEVERANTÖRSADRESS LAND                  
003500*                                 SUPPLIER COUNTRY ADDRESS                
003600     03 IDLEVTLF             PIC X(20)                                    
003700                             VALUE SPACES.                                
003800*                                 TELEFONNUMMER TILL LEVERANTÖR           
003900*                                 TELEPHONE NUMBER TO SUPPLIER            
004000     03 IDLEVFAX             PIC X(20)                                    
004100                             VALUE SPACES.                                
004200*                                 TELEFAXNUMMER TILL LEVERANTÖR           
004300*                                 TELEFAX NUMBER TO THE SUPPLIER          
004400     03 IDLANDX2             PIC X(2)                                     
004500                             VALUE SPACES.                                
004600*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
004700*                                 2-LETTER CODE FOR COUNTRY               
004800*** END OF VILMAII-COPY LENGTH= 213 BYTES                                 
