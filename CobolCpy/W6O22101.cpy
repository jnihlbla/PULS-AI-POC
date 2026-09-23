000100 01  MOD-W6O22101.                                                        
000200*                                 MOD-COPYTEXT                            
000300*                                 FÖR W6O221                              
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDLEVNR-IN       PIC X(5).                                    
000900*                                 LEVERANTÖRNUMMER                        
001000     03 MOD-IDLEVNR-UT       PIC X(5).                                    
001100*                                 LEVERANTÖRNUMMER                        
001200     03 MOD-IDLEVG-IN        PIC X(5).                                    
001300*                                 LEVERANTÖRS GODSADRESS NUMMER           
001400     03 MOD-IDLEVG-UT        PIC X(5).                                    
001500*                                 LEVERANTÖRS GODSADRESS NUMMER           
001600     03 MOD-IDLEVG-ENTER     PIC Z(4)9.                                   
001700*                                 LEVERANTÖRS GODSADRESS NUMMER           
001800     03 MOD-IDLEVG-NEXT      PIC Z(4)9.                                   
001900*                                 LEVERANTÖRS GODSADRESS NUMMER           
002000     03 MOD-IDLEVG-SPAR      PIC Z(4)9.                                   
002100*                                 LEVERANTÖRS GODSADRESS NUMMER           
002200     03 MOD-FLSKPLOT-ATTR    PIC X(2).                                    
002300*                                 MFS ATTRIBUTFÄLT                        
002400     03 MOD-FLSKPLOT         PIC X.                                       
002500*                                 SKIPLOT KONTROLL ?                      
002600     03 MOD-KDCMD-LEV-ATTR   PIC X(2).                                    
002700*                                 MFS ATTRIBUTFÄLT                        
002800     03 MOD-KDCMD-LEV        PIC X.                                       
002900*                                 RAD-UPPDATERINGSKOMMANDO                
003000*                                  BLANK  = INGENTING                     
003100*                                  D , B  = DELETE                        
003200*                                  R , Ä  = REPLACE                       
003300*                                  I , N  = INSERT                        
003400*                                  S , V  = SELECT                        
003500     03 MOD-IDMAIL1-ATTR     PIC X(2).                                    
003600*                                 MFS ATTRIBUTFÄLT                        
003700     03 MOD-IDMAIL1          PIC X(60).                                   
003800*                                 MAIL ADRESS                             
003900     03 MOD-IDMAIL2-ATTR     PIC X(2).                                    
004000*                                 MFS ATTRIBUTFÄLT                        
004100     03 MOD-IDMAIL2          PIC X(60).                                   
004200*                                 MAIL ADRESS                             
004300     03 MOD-IDMAIL3-ATTR     PIC X(2).                                    
004400*                                 MFS ATTRIBUTFÄLT                        
004500     03 MOD-IDMAIL3          PIC X(60).                                   
004600*                                 MAIL ADRESS                             
004700     03 MOD-BELEV-ATTR       PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900     03 MOD-BELEV            PIC X(35).                                   
005000*                                 LEVERANTÖRSNAMN                         
005100     03 MOD-KDCMD-GADR-ATTR  PIC X(2).                                    
005200*                                 MFS ATTRIBUTFÄLT                        
005300     03 MOD-KDCMD-GADR       PIC X.                                       
005400*                                 RAD-UPPDATERINGSKOMMANDO                
005500*                                  BLANK  = INGENTING                     
005600*                                  D , B  = DELETE                        
005700*                                  R , Ä  = REPLACE                       
005800*                                  I , N  = INSERT                        
005900*                                  S , V  = SELECT                        
006000     03 MOD-ADLEV-RAD1-ATTR  PIC X(2).                                    
006100*                                 MFS ATTRIBUTFÄLT                        
006200     03 MOD-ADLEV-RAD1       PIC X(35).                                   
006300*                                 LEVERANTÖRSADRESS                       
006400     03 MOD-ADLEV-RAD2-ATTR  PIC X(2).                                    
006500*                                 MFS ATTRIBUTFÄLT                        
006600     03 MOD-ADLEV-RAD2       PIC X(35).                                   
006700*                                 LEVERANTÖRSADRESS                       
006800     03 MOD-IDLEVFAX-ATTR-1  PIC X(2).                                    
006900*                                 MFS ATTRIBUTFÄLT                        
007000     03 MOD-IDLEVFAX-1       PIC X(16).                                   
007100*                                 TELEFAXNUMMER TILL LEVERANTÖR           
007200     03 MOD-ADLEV-ORT-ATTR   PIC X(2).                                    
007300*                                 MFS ATTRIBUTFÄLT                        
007400     03 MOD-ADLEV-ORT        PIC X(35).                                   
007500*                                 LEVERANTÖRSADRESS ORT                   
007600     03 MOD-IDLEVFAX-ATTR-2  PIC X(2).                                    
007700*                                 MFS ATTRIBUTFÄLT                        
007800     03 MOD-IDLEVFAX-2       PIC X(16).                                   
007900*                                 TELEFAXNUMMER TILL LEVERANTÖR           
008000     03 MOD-ADLEVLND-ATTR    PIC X(2).                                    
008100*                                 MFS ATTRIBUTFÄLT                        
008200     03 MOD-ADLEVLND         PIC X(20).                                   
008300*                                 LEVERANTÖRSADRESS LAND                  
008400     03 MOD-IDLEVFAX-ATTR-3  PIC X(2).                                    
008500*                                 MFS ATTRIBUTFÄLT                        
008600     03 MOD-IDLEVFAX-3       PIC X(16).                                   
008700*                                 TELEFAXNUMMER TILL LEVERANTÖR           
008800     03 MOD-IDLEVTLF-ATTR    PIC X(2).                                    
008900*                                 MFS ATTRIBUTFÄLT                        
009000     03 MOD-IDLEVTLF         PIC X(20).                                   
009100*                                 TELEFONNUMMER TILL LEVERANTÖR           
009200     03 MOD-IDLEVFAX-ATTR-4  PIC X(2).                                    
009300*                                 MFS ATTRIBUTFÄLT                        
009400     03 MOD-IDLEVFAX-4       PIC X(16).                                   
009500*                                 TELEFAXNUMMER TILL LEVERANTÖR           
009600     03 MOD-ADATTENT-Q-ATTR  PIC X(2).                                    
009700*                                 MFS ATTRIBUTFÄLT                        
009800     03 MOD-ADATTENT-Q       PIC X(40).                                   
009900*                                 ATTENTIONADRESS                         
010000     03 MOD-ADATTENT-A-ATTR  PIC X(2).                                    
010100*                                 MFS ATTRIBUTFÄLT                        
010200     03 MOD-ADATTENT-A       PIC X(40).                                   
010300*                                 ATTENTIONADRESS                         
010400     03 MOD-LEVNAMN          PIC X(35).                                   
010500     03 MOD-TEMFSINF         PIC X(55).                                   
010600*                                 INFORMATIONSMEDDELANDE                  
010700*** END OF VILMAII-COPY LENGTH= 712 BYTES                                 
