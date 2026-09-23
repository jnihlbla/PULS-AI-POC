000100 01  MOD-W5O31101.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD 5311              
000300*                                 INVENTERING                             
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDARTNR-IN       PIC Z(8)9.                                   
000900*                                 ARTIKELNUMMER                           
001000     03 MOD-IDARTNR-UT       PIC Z(8)9.                                   
001100*                                 ARTIKELNUMMER                           
001200     03 MOD-KDINVKAT-IN      PIC Z9.                                      
001300*                                 INVENTERINGSKATEGORI                    
001400     03 MOD-KDINVKAT-UT      PIC Z9.                                      
001500*                                 INVENTERINGSKATEGORI                    
001600     03 MOD-DATUM-IN         PIC 9(6).                                    
001700     03 MOD-DATUM-UT         PIC 9(6).                                    
001800     03 MOD-IDUSER-IN        PIC X(8).                                    
001900*                                 ANVÄNDARENS SÄKERHETS ID                
002000     03 MOD-IDUSER-UT        PIC X(8).                                    
002100*                                 ANVÄNDARENS SÄKERHETS ID                
002200     03 MOD-IDDC-IN          PIC X(2).                                    
002300*                                 IDENTIFIERARE LAGER                     
002400     03 MOD-IDDC-UT          PIC X(2).                                    
002500*                                 IDENTIFIERARE LAGER                     
002600     03 MOD-KDARBTYP-ATTR    PIC X(2).                                    
002700*                                 MFS ATTRIBUTFÄLT                        
002800     03 MOD-KDARBTYP-UT      PIC X(4).                                    
002900*                                 TYP AV ARBETE                           
003000     03 MOD-IDPERSON-ATTR    PIC X(2).                                    
003100*                                 MFS ATTRIBUTFÄLT                        
003200     03 MOD-IDPERSON-UT      PIC Z(2)9.                                   
003300*                                 PERSONKOD                               
003400     03 MOD-RAD-INFO         OCCURS 14 TIMES.                             
003500*                                 RAD-INFO                                
003600        05 MOD-RADINFO-UT    PIC X(79).                                   
003700     03 MOD-TEMFSINF         PIC X(55).                                   
003800*                                 INFORMATIONSMEDDELANDE                  
003900*** END OF VILMAII-COPY LENGTH= 1270 BYTES                                
