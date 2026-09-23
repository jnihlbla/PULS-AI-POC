000100 01  MID-W6I32201.                                                        
000200*                                                                         
000300     03 MID-KDARBTYP-IN      PIC X(8).                                    
000400*                                 TYP AV ARBETE                           
000500     03 MID-KDARBTYP-UT      PIC X(8).                                    
000600*                                 TYP AV ARBETE                           
000700     03 MID-IDDC-IN          PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 MID-IDDC-UT          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MID-IDPERSON-FOM-IN  PIC X(3).                                    
001200*                                 PERSONKOD                               
001300     03 MID-IDPERSON-FOM-UT  PIC X(3).                                    
001400*                                 PERSONKOD                               
001500     03 MID-IDPERSON-TOM-IN  PIC X(3).                                    
001600*                                 PERSONKOD                               
001700     03 MID-IDPERSON-TOM-UT  PIC X(3).                                    
001800*                                 PERSONKOD                               
001900     03 MID-IDARTNR-IN       PIC X(9).                                    
002000*                                 ARTIKELNUMMER                           
002100     03 MID-IDARTNR-UT       PIC X(9).                                    
002200*                                 ARTIKELNUMMER                           
002300     03 MID-TIDATUM-IN       PIC X(6).                                    
002400*                                 DATUM ENLIGT KDDATFORM                  
002500     03 MID-TIDATUM-UT       PIC X(6).                                    
002600*                                 DATUM ENLIGT KDDATFORM                  
002700     03 MID-FLKLAR           PIC X.                                       
002800*                                 AVSLUTNINGSMARKERING                    
002900     03 MID-INPUT            OCCURS 12 TIMES.                             
003000*                                                                         
003100        05 MID-CMD           PIC X.                                       
003200        05 MID-IDARTNR       PIC X(9).                                    
003300*                                 ARTIKELNUMMER                           
003400        05 MID-SUARTSTD      PIC X(10).                                   
003500*                                 SUMMA STANDARDPRIS RADVÄRDE             
003600        05 MID-TIDATUM       PIC X(6).                                    
003700*                                 DATUM ENLIGT KDDATFORM                  
003800        05 MID-IDDC          PIC X(2).                                    
003900*                                 IDENTIFIERARE LAGER                     
004000*** END OF VILMAII-COPY LENGTH= 399 BYTES                                 
