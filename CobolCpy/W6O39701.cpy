000100 01  MOD-W6O39701.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD 6397              
000300*                                 GODSMOTTAGNINGSHISTORIK                 
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
001600     03 MOD-INFO-RAD         OCCURS 13 TIMES.                             
001700*                                 RADINFORMATION                          
001800        05 MOD-IDPTYP        PIC X(3).                                    
001900*                                 POSTTYP                                 
002000        05 MOD-IDDC          PIC X(2).                                    
002100*                                 IDENTIFIERARE LAGER                     
002200        05 MOD-IDLEVNR       PIC X(5).                                    
002300*                                 LEVERANTÖRNUMMER                        
002400        05 MOD-KDRT          PIC Z9.                                      
002500*                                 REDOVISNINGSTYP                         
002600        05 MOD-TIINLINL      PIC 9(6).                                    
002700*                                 RAPPORTERINGSDATUM INLAGD (R32)         
002800        05 MOD-KVAVIS        PIC -(6)9.                                   
002900*                                 AVISERAT ANTAL                          
003000        05 MOD-KVANTMOT      PIC -(6)9.                                   
003100*                                 ANTAL MOTTAGET                          
003200        05 MOD-KVART-SKROT   PIC Z(6)9.                                   
003300*                                 ANTAL SKROTADE ARTIKLAR                 
003400        05 MOD-IDUSER-003    PIC X(5).                                    
003500*                                 ANSVARIGT USERID INLÄGGN.(R32)          
003600     03 MOD-TEMFSINF         PIC X(55).                                   
003700*                                 INFORMATIONSMEDDELANDE                  
003800*** END OF VILMAII-COPY LENGTH= 693 BYTES                                 
