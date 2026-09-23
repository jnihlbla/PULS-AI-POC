000100 01  MOD-W2O11101.                                                        
000200*                                 COPYTEXT FÖR MID W2O11101               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDLEVNR-IN       PIC X(5).                                    
000800*                                 LEVERANTÖRNUMMER                        
000900     03 MOD-IDLEVNR-UT       PIC X(5).                                    
001000*                                 LEVERANTÖRNUMMER                        
001100     03 MOD-IDDC-IN          PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300     03 MOD-IDDC-UT          PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 MOD-BELEV-VCC        PIC X(60).                                   
001600*                                 LEVERANTÖRSNAMN VCC                     
001700     03 MOD-ADLEV-RAD1       PIC X(35).                                   
001800*                                 LEVERANTÖRENS GATUADRESS                
001900     03 MOD-ADLEV-RAD2-VCC   PIC X(40).                                   
002000     03 MOD-ADLEV-ORT-VCC    PIC X(40).                                   
002100     03 MOD-ADLEVLND         PIC X(20).                                   
002200*                                 LEVERANTÖRSADRESS LAND                  
002300     03 MOD-IDLANDX2         PIC X(2).                                    
002400*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
002500     03 MOD-IDLEVTLF         PIC X(20).                                   
002600*                                 TELEFONNUMMER TILL LEVERANTÖR           
002700     03 MOD-IDLEVFAX         PIC X(20).                                   
002800*                                 TELEFAXNUMMER TILL LEVERANTÖR           
002900     03 MOD-IDATTENT-ENTER   PIC 9(2).                                    
003000*                                 ATTENTION NUMMER                        
003100     03 MOD-IDATTENT-NEXT    PIC 9(2).                                    
003200*                                 ATTENTION NUMMER                        
003300     03 MOD-ATTENTION        OCCURS 2 TIMES.                              
003400        05 MOD-IDATTENT      PIC Z9.                                      
003500*                                 ATTENTION NUMMER                        
003600        05 MOD-BELEV         PIC X(35).                                   
003700*                                 LEVERANTÖRSNAMN                         
003800        05 MOD-IDMAIL        PIC X(47).                                   
003900*                                 MAIL ADRESS                             
004000        05 MOD-IDLEVTLF-KLEV PIC X(20).                                   
004100*                                 TEL KONTAKTPERS HOS LEVERANTÖR          
004200        05 MOD-TENOTE        PIC X(26).                                   
004300*                                 NOTERINGSFÄLT                           
004400     03 MOD-IDANSK-PG-UT     OCCURS 8 TIMES                               
004500                             PIC Z(2)9.                                   
004600*                                 ANSKAFFARNR PER PLANERINGSGRUPP         
004700     03 MOD-IDANSKPG-IN      OCCURS 8 TIMES.                              
004800        05 MOD-IDANSK-PG-IN-ATTR                                          
004900                             PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100        05 MOD-IDANSK-PG-IN  PIC Z(2)9.                                   
005200*                                 ANSKAFFARNR PER PLANERINGSGRUPP         
005300     03 MOD-IDLEVNR-MOTSV    PIC X(5).                                    
005400*                                 MOTSVARANDE LEVERANTÖRSID               
005500     03 MOD-FLRSADR          PIC X.                                       
005600*                                 RS-UNIK LEV-ADRESS                      
005700     03 MOD-KDLEVTYP         PIC 9.                                       
005800*                                 LEVERANTÖRTYP                           
005900     03 MOD-KVDAGAR-TTC1-UT  PIC Z9.                                      
006000*                                 DAGAR TULL- OCH TRANSPORT-TID           
006100*                                 C1                                      
006200     03 MOD-KVDAGAR-TTC1-IN-ATTR                                          
006300                             PIC X(2).                                    
006400*                                 MFS ATTRIBUTFÄLT                        
006500     03 MOD-KVDAGAR-TTC1-IN  PIC Z9.                                      
006600*                                 DAGAR TULL- OCH TRANSPORT-TID           
006700*                                 C1                                      
006800     03 MOD-KVVECKOR-LT      PIC Z9.                                      
006900*                                 ANTAL VECKOR LEDTID                     
007000     03 MOD-TILEVDAG-IN      OCCURS 5 TIMES.                              
007100        05 MOD-DAG-POS-ATTR  PIC X(2).                                    
007200*                                 MFS ATTRIBUTFÄLT                        
007300        05 MOD-DAG-POS       PIC X(2).                                    
007400     03 MOD-KVVECKOR-LVAR-UT PIC Z9.9.                                    
007500*                                 VARIANS I LEDTIDEN                      
007600     03 MOD-KVVECKOR-LVAR-IN-ATTR                                         
007700                             PIC X(2).                                    
007800*                                 MFS ATTRIBUTFÄLT                        
007900     03 MOD-KVVECKOR-LVAR-IN PIC Z9.9.                                    
008000*                                 VARIANS I LEDTIDEN                      
008100     03 MOD-KVDAGAR-AVIAVV-UT                                             
008200                             PIC Z.                                       
008300*                                 TOLERANSAVVIKELSE FÖRAVISERING          
008400     03 MOD-KVDAGAR-AVIAVV-IN-ATTR                                        
008500                             PIC X(2).                                    
008600*                                 MFS ATTRIBUTFÄLT                        
008700     03 MOD-KVDAGAR-AVIAVV-IN                                             
008800                             PIC Z.                                       
008900*                                 TOLERANSAVVIKELSE FÖRAVISERING          
009000     03 MOD-KVDAGAR-INLAVV-UT                                             
009100                             PIC Z.                                       
009200*                                 TOLERANSAVVIKELSE INLEVERANS            
009300     03 MOD-KVDAGAR-INLAVV-IN-ATTR                                        
009400                             PIC X(2).                                    
009500*                                 MFS ATTRIBUTFÄLT                        
009600     03 MOD-KVDAGAR-INLAVV-IN                                             
009700                             PIC Z.                                       
009800*                                 TOLERANSAVVIKELSE INLEVERANS            
009900     03 MOD-SPAR-DAG         OCCURS 5 TIMES                               
010000                             PIC X(2).                                    
010100     03 MOD-TEMFSINF         PIC X(55).                                   
010200*                                 INFORMATIONSMEDDELANDE                  
010300*** END OF VILMAII-COPY LENGTH= 741 BYTES                                 
