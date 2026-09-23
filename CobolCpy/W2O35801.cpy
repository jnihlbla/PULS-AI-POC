000100 01  MOD-W2O35801.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD                   
000300*                                 ON ORDER FROM LOCAL VENDORS             
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDARTNR-IN       PIC X(9).                                    
000900*                                 ARTIKELNUMMER                           
001000     03 MOD-IDARTNR-UT       PIC X(9).                                    
001100*                                 ARTIKELNUMMER                           
001200     03 MOD-IDDC-IN          PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400     03 MOD-IDDC-UT          PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600     03 MOD-BEART            PIC X(25).                                   
001700*                                 ARTIKELBENÄMNING                        
001800     03 MOD-INFO-RAD         OCCURS 13 TIMES.                             
001900*                                 RADINFORMATION                          
002000        05 MOD-IDDC          PIC X(2).                                    
002100*                                 IDENTIFIERARE LAGER                     
002200        05 MOD-IDLEVNR       PIC X(5).                                    
002300*                                 LEVERANTÖRNUMMER                        
002400        05 MOD-TIREGDAT      PIC 9(6).                                    
002500*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
002600        05 MOD-IDKUNDRF      PIC X(10).                                   
002700*                                 KUNDENS REFERENS (ORDERID)              
002800        05 MOD-KVBEART-UT    PIC Z(6).                                    
002900*                                 BESTÄLLT ANTAL STYCKEN                  
003000        05 MOD-KVBEART-IN-ATTR                                            
003100                             PIC X(2).                                    
003200*                                 MFS ATTRIBUTFÄLT                        
003300        05 MOD-KVBEART-IN    PIC Z(6).                                    
003400*                                 BESTÄLLT ANTAL STYCKEN                  
003500        05 MOD-TIBERANK-UT   PIC X(6).                                    
003600*                                 BERÄKNAD ANKOMSTDATUM                   
003700        05 MOD-TIBERANK-IN-ATTR                                           
003800                             PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000        05 MOD-TIBERANK-IN   PIC X(6).                                    
004100*                                 BERÄKNAD ANKOMSTDATUM                   
004200        05 MOD-TIREGTID      PIC 9(6).                                    
004300*                                 REGISTRERINGSTID                        
004400     03 MOD-TEMFSINF         PIC X(55).                                   
004500*                                 INFORMATIONSMEDDELANDE                  
004600*** END OF VILMAII-COPY LENGTH= 887 BYTES                                 
