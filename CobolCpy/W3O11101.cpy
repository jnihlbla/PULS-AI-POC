000100 01  MOD-W3O11101.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD 3111              
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500*                                 SCREEN NUMBER                           
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800*                                 MFS ERROR MESSAGE                       
000900     03 MOD-IDRADIONR-IN     PIC X(10).                                   
001000*                                 RADIO CHASSI/SERIE NUMMER               
001100*                                 RADIO CHASSI/SERIAL NUMBER              
001200     03 MOD-IDARTNR-IN       PIC X(10).                                   
001300     03 MOD-IDAPPTYP-IN      PIC X(11).                                   
001400*                                 APPARATTYP                              
001500*                                 TYPE OF RADIO/STEREO                    
001600     03 MOD-IDRADIONR-UT     PIC X(10).                                   
001700*                                 RADIO CHASSI/SERIE NUMMER               
001800*                                 RADIO CHASSI/SERIAL NUMBER              
001900     03 MOD-IDARTNR-UT       PIC Z(9)9.                                   
002000*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
002100*                                 SUPPLIERS PART DESCRIPTION              
002200     03 MOD-IDAPPTYP-UT      PIC X(11).                                   
002300*                                 APPARATTYP                              
002400*                                 TYPE OF RADIO/STEREO                    
002500     03 MOD-RADIO-GRUPP      OCCURS 8 TIMES.                              
002600        05 MOD-KDRADIO       PIC X(6).                                    
002700*                                 RADIO SKYDDSKOD                         
002800*                                 RADIO PROTECTION CODE                   
002900        05 MOD-IDARTNRO      PIC Z(9)9.                                   
003000*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
003100*                                 SUPPLIERS PART DESCRIPTION              
003200        05 MOD-IDAPPTYPO     PIC X(11).                                   
003300*                                 APPARATTYP                              
003400*                                 TYPE OF RADIO/STEREO                    
003500     03 MOD-TEMFSINF         PIC X(55).                                   
003600*                                 INFORMATIONSMEDDELANDE                  
003700*                                 INFORMATION MESSAGE                     
003800*** END OF VILMAII-COPY LENGTH= 377 BYTES                                 
