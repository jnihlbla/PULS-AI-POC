000100 01  MOD-W2O11501.                                                        
000200*                                 MOD-COPYTEXT FÖR W2011500               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDLEVNR-IN       PIC X(5).                                    
000800*                                 LEVERANTÖRNUMMER                        
000900     03 MOD-IDLEVNR-UT       PIC X(5).                                    
001000*                                 LEVERANTÖRNUMMER                        
001100     03 MOD-IDATTENT-ENTER   PIC 9(2).                                    
001200*                                 ATTENTION NUMMER                        
001300     03 MOD-IDATTENT-NEXT    PIC 9(2).                                    
001400*                                 ATTENTION NUMMER                        
001500     03 MOD-BELEV-VCC-ATTR   PIC X(2).                                    
001600*                                 MFS ATTRIBUTFÄLT                        
001700     03 MOD-BELEV-VCC        PIC X(60).                                   
001800*                                 LEVERANTÖRSNAMN VCC                     
001900     03 MOD-IDLEVNR-MOTSV    PIC X(5).                                    
002000*                                 MOTSVARANDE LEVERANTÖRSID               
002100     03 MOD-ADLEV-RAD1-ATTR  PIC X(2).                                    
002200*                                 MFS ATTRIBUTFÄLT                        
002300     03 MOD-ADLEV-RAD1       PIC X(35).                                   
002400*                                 LEVERANTÖRENS GATUADRESS                
002500     03 MOD-ADLEV-RAD2-VCC-ATTR                                           
002600                             PIC X(2).                                    
002700*                                 MFS ATTRIBUTFÄLT                        
002800     03 MOD-ADLEV-RAD2-VCC   PIC X(42).                                   
002900*                                 LEVERANTÖRSADRESS RAD2 VCC              
003000     03 MOD-ADLEV-ORT-VCC-ATTR                                            
003100                             PIC X(2).                                    
003200*                                 MFS ATTRIBUTFÄLT                        
003300     03 MOD-ADLEV-ORT-VCC    PIC X(46).                                   
003400*                                 LEVERANTÖRSADRESS ORT VCC               
003500     03 MOD-ADLEVLND-ATTR    PIC X(2).                                    
003600*                                 MFS ATTRIBUTFÄLT                        
003700     03 MOD-ADLEVLND         PIC X(20).                                   
003800*                                 LEVERANTÖRSADRESS LAND                  
003900     03 MOD-IDLEVTLF-ATTR    PIC X(2).                                    
004000*                                 MFS ATTRIBUTFÄLT                        
004100     03 MOD-IDLEVTLF         PIC X(20).                                   
004200*                                 TELEFONNUMMER TILL LEVERANTÖR           
004300     03 MOD-IDLEVFAX-ATTR    PIC X(2).                                    
004400*                                 MFS ATTRIBUTFÄLT                        
004500     03 MOD-IDLEVFAX         PIC X(20).                                   
004600*                                 TELEFAXNUMMER TILL LEVERANTÖR           
004700     03 MOD-IDLANDX2-ATTR    PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900     03 MOD-IDLANDX2         PIC X(2).                                    
005000*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
005100     03 MOD-KVVECKOR-LT      PIC Z9.                                      
005200*                                 ANTAL VECKOR LEDTID                     
005300     03 MOD-KVVECKOR-LT-IN-ATTR                                           
005400                             PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600     03 MOD-KVVECKOR-LT-IN   PIC X(2).                                    
005700*                                 ANTAL VECKOR LEDTID                     
005800     03 MOD-KVVECKOR-AT      PIC Z9.                                      
005900*                                 ANTAL VECKOR ANSKAFFNINGSTID            
006000     03 MOD-KVVECKOR-AT-IN-ATTR                                           
006100                             PIC X(2).                                    
006200*                                 MFS ATTRIBUTFÄLT                        
006300     03 MOD-KVVECKOR-AT-IN   PIC X(2).                                    
006400*                                 ANTAL VECKOR ANSKAFFNINGSTID            
006500     03 MOD-KDLEVTYP         PIC 9.                                       
006600*                                 LEVERANTÖRTYP                           
006700     03 MOD-KDLEVTYP-IN-ATTR PIC X(2).                                    
006800*                                 MFS ATTRIBUTFÄLT                        
006900     03 MOD-KDLEVTYP-IN      PIC 9.                                       
007000*                                 LEVERANTÖRTYP                           
007100     03 MOD-FLRSADR          PIC X.                                       
007200*                                 RS-UNIK LEV-ADRESS                      
007300     03 MOD-FLRSADR-IN-ATTR  PIC X(2).                                    
007400*                                 MFS ATTRIBUTFÄLT                        
007500     03 MOD-FLRSADR-IN       PIC X.                                       
007600*                                 RS-UNIK LEV-ADRESS                      
007700     03 MOD-ATTENTION-RAD    OCCURS 2 TIMES.                              
007800*                                 RADINFORMATION                          
007900        05 MOD-IDATTENT      PIC Z9.                                      
008000*                                 ATTENTION NUMMER                        
008100        05 MOD-BELEV         PIC X(35).                                   
008200*                                 LEVERANTÖRSNAMN                         
008300        05 MOD-IDMAIL        PIC X(50).                                   
008400*                                 MAIL ADRESS                             
008500        05 MOD-IDLEVTLF-KLEV PIC X(20).                                   
008600*                                 TEL KONTAKTPERS HOS LEVERANTÖR          
008700        05 MOD-TENOTE        PIC X(35).                                   
008800*                                 NOTERINGSFÄLT                           
008900     03 MOD-IDATTENT-IN-ATTR PIC X(2).                                    
009000*                                 MFS ATTRIBUTFÄLT                        
009100     03 MOD-IDATTENT-IN      PIC Z9.                                      
009200*                                 ATTENTION NUMMER                        
009300     03 MOD-BELEV-UPD-ATTR   PIC X(2).                                    
009400*                                 MFS ATTRIBUTFÄLT                        
009500     03 MOD-BELEV-UPD        PIC X(35).                                   
009600*                                 LEVERANTÖRSNAMN                         
009700     03 MOD-IDMAIL-UPD-ATTR  PIC X(2).                                    
009800*                                 MFS ATTRIBUTFÄLT                        
009900     03 MOD-IDMAIL-UPD       PIC X(50).                                   
010000*                                 MAIL ADRESS                             
010100     03 MOD-IDLEVTLF-KLEV-UPD-ATTR                                        
010200                             PIC X(2).                                    
010300*                                 MFS ATTRIBUTFÄLT                        
010400     03 MOD-IDLEVTLF-KLEV-UPD                                             
010500                             PIC X(20).                                   
010600*                                 TEL KONTAKTPERS HOS LEVERANTÖR          
010700     03 MOD-TENOTE-UPD-ATTR  PIC X(2).                                    
010800*                                 MFS ATTRIBUTFÄLT                        
010900     03 MOD-TENOTE-UPD       PIC X(35).                                   
011000     03 MOD-FLAGGA-BORT-ATTR PIC X(2).                                    
011100*                                 MFS ATTRIBUTFÄLT                        
011200     03 MOD-FLAGGA-BORT      PIC X.                                       
011300*                                 ALLMÄN SVARSFLAGGA                      
011400     03 MOD-IDATTENT-UPD-ATTR                                             
011500                             PIC X(2).                                    
011600*                                 MFS ATTRIBUTFÄLT                        
011700     03 MOD-IDATTENT-UPD     PIC Z9.                                      
011800*                                 ATTENTION NUMMER                        
011900     03 MOD-TEMFSINF         PIC X(55).                                   
012000*                                 INFORMATIONSMEDDELANDE                  
012100*** END OF VILMAII-COPY LENGTH= 842 BYTES                                 
