000100 01  MOD-W5O31201.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD 5312              
000300*                                 INVENTERING                             
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDARTNR-IN       PIC Z(8)9.                                   
000900*                                 ARTIKELNUMMER                           
001000     03 MOD-IDARTNR-UT       PIC Z(8)9.                                   
001100*                                 ARTIKELNUMMER                           
001200     03 MOD-KDJUSTYP-IN      PIC 9.                                       
001300*                                 JUSTERINGSTYP                           
001400     03 MOD-KDJUSTYP-UT      PIC 9.                                       
001500*                                 JUSTERINGSTYP                           
001600     03 MOD-IDDC-IN          PIC X(2).                                    
001700*                                 IDENTIFIERARE LAGER                     
001800     03 MOD-IDDC-UT          PIC X(2).                                    
001900*                                 IDENTIFIERARE LAGER                     
002000     03 MOD-KDARBTYP-ATTR    PIC X(2).                                    
002100*                                 MFS ATTRIBUTFÄLT                        
002200     03 MOD-KDARBTYP-UT      PIC X(4).                                    
002300*                                 TYP AV ARBETE                           
002400     03 MOD-IDPERSON-ATTR    PIC X(2).                                    
002500*                                 MFS ATTRIBUTFÄLT                        
002600     03 MOD-IDPERSON-UT      PIC Z(2)9.                                   
002700*                                 PERSONKOD                               
002800     03 MOD-RAD-INFO         OCCURS 14 TIMES.                             
002900*                                 RAD-INFO                                
003000        05 MOD-RADINFO-UT    PIC X(79).                                   
003100     03 MOD-TEMFSINF         PIC X(55).                                   
003200*                                 INFORMATIONSMEDDELANDE                  
003300*** END OF VILMAII-COPY LENGTH= 1240 BYTES                                
