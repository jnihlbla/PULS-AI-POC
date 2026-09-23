000100 01  MOD-W3O12501.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD 3125              
000300*                                 BYTES CODES                             
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-DATA-UT          OCCURS 10 TIMES.                             
000900*                                 RAPPORTERINGS-FÄLT                      
001000        05 MOD-TAB-KDBYTREF  PIC X(3).                                    
001100*                                 CENTRAL REFERENS                        
001200        05 MOD-TAB-TENOTE    PIC X(40).                                   
001300*                                 NOTERINGSFÄLT                           
001400        05 MOD-TAB-KVPOINT   PIC Z(5)9.                                   
001500*                                 POINT VALUE                             
001600     03 MOD-DATA-IN.                                                      
001700*                                 INMATNINGSFÄLT                          
001800        05 MOD-FLUPDAT-ATTR  PIC X(2).                                    
001900*                                 MFS ATTRIBUTFÄLT                        
002000        05 MOD-FLUPDAT       PIC X.                                       
002100*                                 ALLMÄN FLAGGA                           
002200        05 MOD-KDBYTREF-ATTR PIC X(2).                                    
002300*                                 MFS ATTRIBUTFÄLT                        
002400        05 MOD-KDBYTREF      PIC X(3).                                    
002500*                                 CENTRAL REFERENS                        
002600        05 MOD-TENOTE-ATTR   PIC X(2).                                    
002700*                                 MFS ATTRIBUTFÄLT                        
002800        05 MOD-TENOTE        PIC X(40).                                   
002900*                                 NOTERINGSFÄLT                           
003000        05 MOD-KVPOINT-ATTR  PIC X(2).                                    
003100*                                 MFS ATTRIBUTFÄLT                        
003200        05 MOD-KVPOINT       PIC Z(5)9.                                   
003300*                                 POINT VALUE                             
003400     03 MOD-TEMFSINF         PIC X(55).                                   
003500*                                 INFORMATIONSMEDDELANDE                  
003600*** END OF VILMAII-COPY LENGTH= 647 BYTES                                 
